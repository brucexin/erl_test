%%test mnesia for ordered_set
-module(mnesia_test).

-compile(export_all).

-record(prior_key, {order, ts=erlang:now()}).
-record(test_data, {key, phone, memo, addr}).

create_tables() ->
    %Nodes = [node()],
    %mnesia:create_schema(Nodes),
    mnesia:create_table(test_data, 
        [
            {attributes, record_info(fields, test_data)}, 
            {index, [phone, addr]},
            {type, ordered_set}
        ]).

ensure_loaded() ->
    ok = mnesia:wait_for_tables([test_data], 5000).

insert_data(Order, Phone, Addr) ->
    ok = mnesia:dirty_write(#test_data{
            key=#prior_key{order=Order}, 
            phone=Phone, 
            memo="aaa", 
            addr=Addr}),
    ok.

rec_test(Data=#test_data{}) ->
    io:format("Data:~p~n", [Data]).

insert_batch(0) ->
    ok;
insert_batch(Count) ->
    insert_data(crypto:rand_uniform(0, 1000), 
        crypto:rand_uniform(0, 1000), 
        crypto:rand_uniform(1000, 2000)),
    insert_batch(Count-1).


