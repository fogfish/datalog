## @author     Dmitry Kolesnikov, <dmkolesnikov@gmail.com>
## @copyright  (c) 2012 - 2014 Dmitry Kolesnikov. All Rights Reserved
##
## @description
##   Makefile to build and release Erlang applications using
##   rebar, reltool, etc (see README for details)
##
##   application version schema (based on semantic version)
##   ${APP}-${VSN}+${GIT}.${ARCH}.${PLAT}
##
## @version 0.8.3
.PHONY: test rel deps all pkg

#####################################################################
##
## application config
##
#####################################################################
ROOT  = `pwd`
PREFIX ?= /usr/local
APP ?= $(notdir $(CURDIR))
ARCH?= $(shell uname -m)
PLAT?= $(shell uname -s)
HEAD?= $(shell git rev-parse --short HEAD)
TAG  = ${HEAD}.${ARCH}.${PLAT}
TEST?= ${APP}
S3   =
GIT ?= 
VMI  = 
NET ?= lo0
USER =
PASS =

## root path to benchmark framework
BB     = ../basho_bench
SSHENV = /tmp/ssh-agent.conf
ADDR   = $(shell ifconfig ${NET} | sed -En 's/.*inet (addr:)?(([0-9]*\.){3}[0-9]*).*/\2/p')
BRANCH = $(shell git symbolic-ref --short -q HEAD)

## erlang flags (make run only)
EFLAGS = \
	-name ${APP}@${ADDR} \
	-setcookie nocookie \
	-pa ${ROOT}/ebin \
	-pa ${ROOT}/priv \
	-pa ${ROOT}/deps/*/ebin \
	-pa ${ROOT}/apps/*/ebin \
	-pa rel/files \
	-kernel inet_dist_listen_min 32100 \
	-kernel inet_dist_listen_max 32199 \
	+P 1000000 \
	+K true +A 160 -sbt ts

#####################################################################
##
## internal config
##
#####################################################################
ifeq ($(wildcard rel/reltool.config),) 
	REL =
	VSN =
	TAR =
	PKG =
else
   IID  = $(shell cat rel/reltool.config | sed -n 's/{target_dir,.*\"\([^-]*\).*\"}./\1/p')
	REL  = $(shell cat rel/reltool.config | sed -n 's/{target_dir,.*\"\(.*\)\"}./\1/p')
	VSN  = $(shell echo ${REL} | sed -n 's/.*-\(.*\)/\1/p')
ifeq (${VSN},)
	VSN  = $(shell cat rel/reltool.config | sed -n 's/.*{rel,.*\".*\",.*\"\(.*\)\".*/\1/p')
endif
ifeq (${config},)
	RFLAGS  =	
	VARIANT =
else
	VARIANT = $(addprefix ., $(notdir $(basename ${config})))
	RFLAGS  = target_dir=${REL}${VARIANT} overlay_vars=${ROOT}/${config}
endif
ifeq (${VSN},)
	TAR = ${IID}${VARIANT}+${TAG}.tgz
	PKG = ${IID}${VARIANT}+${TAG}.bundle
else
	TAR = ${IID}-${VSN}${VARIANT}+${TAG}.tgz
	PKG = ${IID}-${VSN}${VARIANT}+${TAG}.bundle
endif
endif

## self-extracting bundle wrapper
BUNDLE_INIT = PREFIX=${PREFIX}\nREL=${PREFIX}/${REL}${VARIANT}\nAPP=${APP}\nVSN=${VSN}\nLINE=\`grep -a -n 'BUNDLE:\x24' \x240\`\ntail -n +\x24(( \x24{LINE\x25\x25:*} + 1)) \x240 | gzip -vdc - | tar -C ${PREFIX} -xvf - > /dev/null\n
BUNDLE_FREE = exit\nBUNDLE:\n
BUILDER = cd /tmp && git clone -b ${BRANCH} ${GIT}/${APP} && cd /tmp/${APP} && make && make rel && sleep 300

#####################################################################
##
## build
##
#####################################################################
all: rebar deps compile

compile:
	@./rebar compile

deps:
	@./rebar get-deps

clean:
	@./rebar clean ; \
	rm -rf test.*-temp-data ; \
	rm -rf tests ; \
	rm -rf log ; \
	rm -f  *.tgz ; \
	rm -f  *.bundle


distclean: clean 
	@./rebar delete-deps

unit: all
	@./rebar skip_deps=true eunit

test:
	@erl ${EFLAGS} -run deb test test/${TEST}.config

docs:
	@./rebar skip_deps=true doc

#####################################################################
##
## release
##
#####################################################################
ifneq (${REL},)

rel: ${TAR}

## assemble VM release
ifeq (${PLAT},$(shell uname -s))
${TAR}:
	@./rebar generate ${RFLAGS} ;\
	cd rel ;\
	test -d ${REL}${VARIANT} && tar -zcf ${TAR} ${REL}${VARIANT}/ ;\
	test -d ${REL}${VARIANT} && mv ${TAR} ../${TAR} ;\
	cd - ;\
	test -f ${TAR} && echo "==> tarball: ${TAR}"

else
ifneq (${VMI},)
${TAR}:
	@echo "==> docker run ${VMI}" ;\
	K=`test ${PASS} && cat  ${PASS}` ;\
	A=`test ${USER} && echo "mkdir -p /root/.ssh && echo \"$$K\" > /root/.ssh/id_rsa && chmod 0700 /root/.ssh/id_rsa && echo -e \"Host *\n\tUser ${USER}\n\tStrictHostKeyChecking no\n\" > /root/.ssh/config &&"` ;\
	I=`docker run -d -a stdout -a stderr ${VMI} /bin/sh -c "$$A ${BUILDER}"` ;\
	(docker attach $$I &) ;\
	docker cp $$I:/tmp/${APP}/${TAR} . 1> /dev/null 2>&1 ;\
	while [ $$? -ne 0 ] ;\
	do \
   	sleep 10 ;\
   	docker cp $$I:/tmp/${APP}/${TAR} . 1> /dev/null 2>&1 ;\
	done ;\
	docker kill $$I ;\
	docker rm $$I

endif
endif

## package VM release to executable bundle
pkg: rel/deploy.sh ${TAR}
	@printf "${BUNDLE_INIT}"  > ${PKG} ; \
	cat  rel/deploy.sh       >> ${PKG} ; \
	printf  "${BUNDLE_FREE}" >> ${PKG} ; \
	cat  ${TAR}              >> ${PKG} ; \
	chmod ugo+x  ${PKG}                ; \
	echo "==> bundle: ${PKG}"

## copy 'package' to s3
## copy 'package' to s3
s3: ${PKG}
	aws s3 cp ${PKG} ${S3}/${APP}+${TAG}${VARIANT}.bundle

s3-latest: ${PKG}
	aws s3 cp ${PKG} ${S3}/${APP}+latest${VARIANT}.bundle
endif

#####################################################################
##
## deploy
##
#####################################################################
ifneq (${host},)
${SSHENV}:
	@echo "==> ssh: config keys" ;\
	ssh-agent -s > ${SSHENV}

node: ${SSHENV}
	@echo "==> deploy: ${host}" ;\
	. ${SSHENV} ;\
	k=`basename ${pass}` ;\
	l=`ssh-add -l | grep $$k` ;\
	if [ -z "$$l" ] ; then \
		ssh-add ${pass} ;\
	fi ;\
	rsync -cav --rsh=ssh --progress ${PKG} ${host}:${PKG} ;\
	ssh -t ${host} "sudo sh ./${PKG}"

endif

#####################################################################
##
## debug
##
#####################################################################
run:
	@erl ${EFLAGS}

benchmark:
	@echo "==> benchmark: ${TEST}" ;\
	$(BB)/basho_bench -N bb@127.0.0.1 -C nocookie priv/${TEST}.benchmark ;\
	$(BB)/priv/summary.r -i tests/current ;\
	open tests/current/summary.png

ifneq (${REL},)
start: 
	@./rel/${REL}${VARIANT}/bin/${APP} start

stop:
	@./rel/${REL}${VARIANT}/bin/${APP} stop

console: 
	@./rel/${REL}${VARIANT}/bin/${APP} console

attach:
	@./rel/${REL}${VARIANT}/bin/${APP} attach
endif

#####################################################################
##
## dependencies
##
#####################################################################
rebar:
	@curl -L -O https://github.com/rebar/rebar/wiki/rebar ; \
	chmod ugo+x rebar
