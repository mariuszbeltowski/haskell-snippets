-module (zad3).
-export ([start/0,client/0,server/0]).


client () ->
  receive
    {X, D, ServerPid} ->
      io:format("~p~n",[X]),

      NewX = X+D,

      if NewX > 100 ; NewX < -100 ->
        ServerPid ! {NewX, -D, self()};
      true ->
        ServerPid ! {NewX, D, self()}
      end,

      client()
  end.

server () ->
  receive
    {X, D, ClientPid} ->

      io:format("~p~n",[X]),

      NewX = X+D,

      if NewX > 100 ; NewX < -100 ->
        ClientPid ! {NewX, -D, self()};
      true ->
        ClientPid ! {NewX, D, self()}
      end,

      server()
  end.

start () ->
	ServerPid = spawn(zad3, server, []),
	ClientPid = spawn (zad3, client, []),
  ClientPid ! {1, 2, ServerPid}.
