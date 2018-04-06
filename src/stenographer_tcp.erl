-module(stenographer_tcp).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/0, send/1]).

% interface

start_link() ->
    gen_server:start_link({local, stenographer}, ?MODULE, [], []).

send(Data) ->
    gen_server:cast(stenographer, {send, Data}).

% implementation

init([]) ->
    {ok, do_connect()}.

handle_cast({send, _Data}, undefined) ->
    {noreply, undefined};
handle_cast({send, Data}, Sock) ->
    ok = gen_tcp:send(Sock, <<Data/binary, $\r, $\n>>),
    {noreply, Sock};
handle_cast(_Msg, S) ->
    {noreply, S}.

handle_info(connect, undefined) ->
    {noreply, do_connect()};
handle_info({tcp_closed, _}, S) ->
    {stop, tcp_closed, S};
handle_info(_Info, S) ->
    error_logger:info_msg("handle_info ~p", [_Info]),
    {noreply, S}.

handle_call(_Request, _From, S) ->
    {reply, ok, S}.

terminate(_Reason, undefined) ->
    ok;
terminate(_Reason, Sock) ->
    ok = gen_tcp:close(Sock),
    ok.

code_change(_OldVsn, S, _Extra) ->
    {ok, S}.

% private

reconnect_interval() ->
    {ok, Interval} = application:get_env(reconnect_interval),
    Interval.

timer() ->
    timer:send_after(reconnect_interval(), connect).

do_connect() ->
    {ok, Host} = application:get_env(host),
    {ok, Port} = application:get_env(port),
    case gen_tcp:connect(Host, Port, [binary, {packet, 0}]) of
        {ok, Sock} ->
            error_logger:info_msg("[~p] successfully connected to influx on ~p:~p", [?MODULE, Host, Port]),
            Sock;
        Error ->
            error_logger:info_msg("[~p] error ~p while connecting to influx on ~p:~p", [?MODULE, Error, Host, Port]),
            timer(),
            undefined
    end.
