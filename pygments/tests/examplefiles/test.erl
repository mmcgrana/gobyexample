-module(test).
-export([listen/1,
         handle_client/1,
         maintain_clients/1,
         start/1,
         stop/0,
         controller/1]).

-author("jerith").

-define(TCP_OPTIONS,[list, {packet, 0}, {active, false}, {reuseaddr, true}]).

-record(player, {name=none, socket, mode}).

%% To allow incoming connections, we need to listen on a TCP port.
%% This is also the entry point for our server as a whole, so it
%% starts the client_manager process and gives it a name so the rest
%% of the code can get to it easily.

listen(Port) ->
    {ok, LSocket} = gen_tcp:listen(Port, ?TCP_OPTIONS),
    register(client_manager, spawn(?MODULE, maintain_clients, [[]])),
    do_accept(LSocket).

%% Accepting a connection gives us a connection socket with the
%% newly-connected client on the other end.  Since we want to accept
%% more than one client, we spawn a new process for each and then wait
%% for another connection on our listening socket.

do_accept(LSocket) ->
    case gen_tcp:accept(LSocket) of
        {ok, Socket} ->
            spawn(?MODULE, handle_client, [Socket]),
            client_manager ! {connect, Socket};
        {error, Reason} ->
            io:format("Socket accept error: ~s~n", [Reason])
    end,
    do_accept(LSocket).

%% All the client-socket process needs to do is wait for data and
%% forward it to the client_manager process which decides what to do
%% with it.  If the client disconnects, we let client_manager know and
%% then quietly go away.

handle_client(Socket) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, Data} ->
            client_manager ! {data, Socket, Data},
            handle_client(Socket);
        {error, closed} ->
            client_manager ! {disconnect, Socket}
    end.

%% This is the main loop of the client_manager process.  It maintains
%% the list of "players" and calls the handler for client input.

maintain_clients(Players) ->
    io:format("Players:~n", []),
    lists:foreach(fun(P) -> io:format(">>> ~w~n", [P]) end, Players),
    receive
        {connect, Socket} ->
            Player = #player{socket=Socket, mode=connect},
            send_prompt(Player),
            io:format("client connected: ~w~n", [Player]),
            NewPlayers =  [Player | Players];
        {disconnect, Socket} ->
            Player = find_player(Socket, Players),
            io:format("client disconnected: ~w~n", [Player]),
            NewPlayers = lists:delete(Player, Players);
        {data, Socket, Data} ->
            Player = find_player(Socket, Players),
            NewPlayers = parse_data(Player, Players, Data),
            NewPlayer = find_player(Socket, NewPlayers),
            send_prompt(NewPlayer)
    end,
    maintain_clients(NewPlayers).

%% find_player is a utility function to get a player record associated
%% with a particular socket out of the player list.

find_player(Socket, Players) ->
    {value, Player} = lists:keysearch(Socket, #player.socket, Players),
    Player.

%% delete_player returns the player list without the given player.  It
%% deletes the player from the list based on the socket rather than
%% the whole record because the list might hold a different version.

delete_player(Player, Players) ->
    lists:keydelete(Player#player.socket, #player.socket, Players).

%% Sends an appropriate prompt to the player.  Currently the only
%% prompt we send is the initial "Name: " when the player connects.

send_prompt(Player) ->
    case Player#player.mode of
        connect ->
            gen_tcp:send(Player#player.socket, "Name: ");
        active ->
            ok
    end.

%% Sends the given data to all players in active mode.

send_to_active(Prefix, Players, Data) ->
    ActivePlayers = lists:filter(fun(P) -> P#player.mode == active end,
                                 Players),
    lists:foreach(fun(P) -> gen_tcp:send(P#player.socket, Prefix ++ Data) end,
                  ActivePlayers),
    ok.

%% We don't really do much parsing, but that will probably change as
%% more features are added.  Currently this handles naming the player
%% when he first connects and treats everything else as a message to
%% send.

parse_data(Player, Players, Data) ->
    case Player#player.mode of
        active ->
            send_to_active(Player#player.name ++ ": ",
              delete_player(Player, Players), Data),
            Players;
        connect ->
            UPlayer = Player#player{name=bogostrip(Data), mode=active},
            [UPlayer | delete_player(Player, Players)]
    end.

%% Utility methods to clean up the name before we apply it.  Called
%% bogostrip rather than strip because it returns the first continuous
%% block of non-matching characters rather stripping matching
%% characters off the front and back.

bogostrip(String) ->
    bogostrip(String, "\r\n\t ").

bogostrip(String, Chars) ->
    LStripped = string:substr(String, string:span(String, Chars)+1),
    string:substr(LStripped, 1, string:cspan(LStripped, Chars)).

%% Here we have some extra code to test other bits of pygments' Erlang
%% lexer.

get_timestamp() ->
    {{Year,Month,Day},{Hour,Min,Sec}} = erlang:universaltime(),
    lists:flatten(io_lib:format(
                    "~4.10.0B-~2.10.0B-~2.10.0BT~2.10.0B:~2.10.0B:~2.10.0BZ",
                    [Year, Month, Day, Hour, Min, Sec])).

a_binary() ->
    << 100:16/integer, 16#7f >>.

a_list_comprehension() ->
    [X*2 || X <- [1,2,3]].

map(Fun, [H|T]) ->
    [Fun(H) | map(Fun, T)];

map(Fun, []) ->
    [].

%% pmap, just because it's cool.

pmap(F, L) ->
    Parent = self(),
    [receive {Pid, Result} ->
             Result
     end || Pid <- [spawn(fun() ->
                                  Parent ! {self(), F(X)} 
                          end) || X <- L]].
