-module(gen_muc_storage).
-author('eric@ohmforce.com').

-export([behaviour_info/1]).

behaviour_info(callbacks) ->
    [{init, 3}, %Host, ServerHost, Opts
    {store_room, 5}, %Host, ServerHost, Name, Type, Conf
    {restore_room,3}, %Host, ServerHost, Name
    {forget_room, 3}, % idem
    {fetch_all_rooms, 2} %Host, ServerHost 
    ];
behaviour_info(_Other) ->
    undefined.