-module(gen_muc_storage).
-author('eric@ohmforce.com').

-export([behaviour_info/1]).

behaviour_info(callbacks) ->
    [{init, 3}, %Host, ServerHost, Opts
    {store_room, 6}, %Host, ServerHost, Name, Type, Conf
    {restore_room,4}, %Host, ServerHost, Name
    {forget_room, 4}, % idem
    {fetch_room_names, 3} %Host, ServerHost 
    ];
behaviour_info(_Other) ->
    undefined.