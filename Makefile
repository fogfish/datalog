##
## @doc
##   an example Makefile to build and ship erlang software
##
##   APP - identity of the application
##   ORG - identity of the organization 
##   URI - identity of the docker repository with last /  

APP = datalog 
ORG = fogfish
URI = 

include erlang.mk
