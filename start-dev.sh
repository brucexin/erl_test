#!/bin/sh
# NOTE: mustache templates need \ because they are not awesome.
exec erl -pa ebin -boot start_sasl \
    -name uuserver_dev@10.6.5.4 \
    -s testapp \
    -setcookie testapp 
