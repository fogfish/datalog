## @author     Dmitry Kolesnikov, <dmkolesnikov@gmail.com>
## @copyright  (c) 2012 - 2014 Dmitry Kolesnikov. All Rights Reserved
##
## @description
##   Makefile to build and release Erlang applications using standard development tools
##
## @version 0.11.5

#####################################################################
##
## application config
##
#####################################################################
PREFIX ?= /usr/local
APP    ?= $(notdir $(CURDIR))
ARCH   ?= $(shell uname -m)
PLAT   ?= $(shell uname -s)
VSN    ?= $(shell test -z "`git status --porcelain`" && git describe --tags --long | sed -e 's/-g[0-9a-f]*//' | sed -e 's/-0//' || echo "`git describe --abbrev=0 --tags`-SNAPSHOT")
REL     = ${APP}-${VSN}
PKG    ?= ${REL}+${ARCH}.${PLAT}
TEST   ?= ${APP}
S3     ?=
VMI    ?= fogfish/erlang
NET    ?= lo0
URL 	 ?= undefined
LATEST ?= latest

## root path to benchmark framework
BB     = ../basho_bench
SSHENV = /tmp/ssh-agent.conf
COOKIE?= nocookie

## erlang runtime flags use by `make run`
ROOT   = $(shell pwd)
ADDR   = $(shell ifconfig ${NET} | sed -En 's/^${NET}:.*inet (addr:)?(([0-9]*\.){3}[0-9]*).*/\2/p' && echo "127.0.0.1")
EFLAGS = \
	-name ${APP}@${ADDR} \
	-setcookie ${COOKIE} \
	-pa ${ROOT}/_build/default/lib/*/ebin \
	-pa ${ROOT}/rel \
	-kernel inet_dist_listen_min 32100 \
	-kernel inet_dist_listen_max 32199 \
	+P 1000000 \
	+K true +A 160 -sbt ts

## self-extracting bundle wrapper
BUNDLE_INIT = PREFIX=${PREFIX}\nREL=${PREFIX}/${REL}\nAPP=${APP}\nVSN=${VSN}\nLINE=`grep -a -n "BUNDLE:$$" $$0`\nmkdir -p $${REL}\ntail -n +$$(( $${LINE%%%%:*} + 1)) $$0 | gzip -vdc - | tar -C $${REL} -xvf - > /dev/null\n
BUNDLE_FREE = exit\nBUNDLE:\n
BUILDER = FROM ${VMI}\nRUN mkdir ${APP}\nCOPY . ${APP}/\nRUN cd ${APP} && make && make rel\n
CTRUN   = \
	-module(test). \
	-export([run/1]). \
	run(Spec) -> \
   	{ok, Test} = file:consult(Spec), \
   	case lists:keyfind(node, 1, Test) of \
      	false -> ct:run_test([{spec, Spec}]); \
         true  -> ct_master:run(Spec) \
   	end, \
		erlang:halt().

#####################################################################
##
## build
##
#####################################################################
all: rebar3 compile

compile:
	@./rebar3 compile

clean:
	@./rebar3 clean ;\
	rm -Rf _build/default/rel ;\
	rm -rf test.*-temp-data ;\
	rm -rf tests ;\
	rm -rf log ;\
	rm -f  relx.config ;\
	rm -f  *.tar.gz ;\
	rm -f  *.bundle

distclean: clean 
	@./rebar3 unlock ;\
	rm -Rf _build ;\
	rm -Rf rebar3

##
## execute unit test
unit: all
	@./rebar3 skip_deps=true eunit

##
## execute common test and terminate node
test: _build/test.beam
	@mkdir -p /tmp/test/${APP} ;\
	erl ${EFLAGS} -pa _build/ -pa test/ -run test run test/${TEST}.config

_build/test.beam: _build/test.erl
	erlc -o _build $<

_build/test.erl:
	echo "${CTRUN}" > $@


#####################################################################
##
## release 
##
#####################################################################
rel: ${PKG}.tar.gz

## assemble VM release
ifeq (${PLAT},$(shell uname -s))
${PKG}.tar.gz: relx.config
	@./rebar3 tar -n ${APP} -v ${VSN} ;\
	cp _build/default/rel/${APP}/${APP}-${VSN}.tar.gz $@ ;\
	echo "==> tarball: $@"

relx.config: rel/relx.config.src
	@cat $< | sed 's/release/release, {${APP}, "${VSN}"}/' > $@ 
else
${PKG}.tar.gz: _build/dockermake
	@docker build --file=$< --force-rm=true	--tag=build/${APP}:latest . ;\
	I=`docker create build/${APP}:latest` ;\
	docker cp $$I:/${APP}/$@ $@ ;\
	docker rm -f $$I ;\
	docker rmi build/${APP}:latest ;\
	test -f $@ && echo "==> tarball: $@"

_build/dockermake:
	@echo "${BUILDER}" > $@
endif

## build docker image
docker: rel/Dockerfile
	docker build \
		--build-arg APP=${APP} \
		--build-arg VSN=${VSN} \
		-t ${URL}/${APP}:${VSN} -f $< .
	docker tag -f ${URL}/${APP}:${VSN} ${URL}/${APP}:${LATEST}



#####################################################################
##
## package / bundle
##
#####################################################################
pkg: ${PKG}.tar.gz ${PKG}.bundle

${PKG}.bundle: rel/deploy.sh
	@printf '${BUNDLE_INIT}' > $@ ;\
	cat $<  >> $@ ;\
	printf  '${BUNDLE_FREE}' >> $@ ;\
	cat  ${PKG}.tar.gz >> $@ ;\
	chmod ugo+x $@ ;\
	echo "==> bundle: $@"

## copy 'package' to s3
s3: ${PKG}.bundle
	aws s3 cp $< ${S3}/$<

s3-latest: ${PKG}.bundle
	aws s3 cp $< ${S3}/${APP}-latest${VARIANT}.bundle

#####################################################################
##
## deploy
##
#####################################################################
ifneq (${host},)
${SSHENV}:
	@echo "==> ssh: config keys" ;\
	ssh-agent -s > ${SSHENV}

node: ${PKG}.bundle ${SSHENV}
	@echo "==> deploy: ${host}" ;\
	. ${SSHENV} ;\
	k=`basename ${pass}` ;\
	l=`ssh-add -l | grep $$k` ;\
	if [ -z "$$l" ] ; then \
		ssh-add ${pass} ;\
	fi ;\
	rsync -cav --rsh=ssh --progress $< ${host}:$< ;\
	ssh -t ${host} "sudo sh ./$<"
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

console: ${PKG}.tar.gz
	@_build/default/rel/${APP}/bin/${APP} console

#####################################################################
##
## dependencies
##
#####################################################################
rebar3:
	@curl -L -O https://s3.amazonaws.com/rebar3/rebar3 ; \
	chmod ugo+x $@

.PHONY: test rel deps all pkg 

