-module(discord_gateway).

-behaviour(gen_server).

-export([start_link/1]).
-export([init/1, terminate/2, handle_call/3, handle_cast/2, handle_info/2]).

-include("discord.hrl").

-define(MAX_RETIRES, 5).
-define(BROWSER, <<"baldur">>).

-record(connection, { pid :: pid(),
                      host :: string(),
                      args :: string(),
                      mref :: reference(),
                      stream=undefined :: reference() | undefined
                    }).

-record(state, { token :: binary(),
                 retries :: non_neg_integer(),
                 connection :: #connection{}
               }).

-record(payload, {op :: non_neg_integer(),
                  d :: #{any() => any()},
                  s=undefined :: non_neg_integer() | undefined,
                  t=undefined :: binary() | undefined
                 }).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec start_link(binary()) -> {ok, pid()} | ignore | {error, term()}.
start_link(Token) ->
    gen_server:start_link(?MODULE, #state{token=Token}, []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% gen_server callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init(State) ->
    gen_server:cast(self(), {connect, ?GATEWAY_HOST, ?GATEWAY_ARGS}),
    {ok, State#state{retries=?MAX_RETIRES}}.

terminate(_Reason, _State) ->
    ok.

handle_call(_Msg, _Fromom, State) ->
    {noreply, State}.

handle_cast({connect, Host, Args}, State) ->
    {ok, ConnPid} = gun:open(Host, ?GATEWAY_PORT, #{protocols => [http]}),
    {ok, _Protocol} = gun:await_up(ConnPid),
    MRef = monitor(process, ConnPid),
    gun:ws_upgrade(ConnPid, Args, []),
    logger:info("connected to ~s", [Host]),
    Connection = #connection{pid=ConnPid,
                             mref=MRef,
                             host=Host,
                             args=Args
                            },
    {noreply, State#state{connection=Connection}};
handle_cast(reconnect, #state{retries=0}) ->
    throw(max_retries_exceeded);
handle_cast(reconnect, State=#state{connection=Conn, retries=Retries}) ->
    logger:info("reconnecting"),
    gun:close(Conn#connection.pid),
    gen_server:cast(self(), {connect,
                             Conn#connection.host,
                             Conn#connection.args}),
    {noreply, State#state{retries = Retries - 1}};
handle_cast(identify, State=#state{token=Token, connection=Conn}) ->
    Ident = #{<<"token">> => Token,
              <<"properties">> => #{
                  <<"$os">> => generate_os_string(),
                  <<"$browser">> => ?BROWSER,
                  <<"$device">> => ?BROWSER
                 }
             },
    Msg = jiffy:encode(#{<<"op">> => 2, <<"d">> => Ident}),
    logger:info("sending identify"),
    gun:ws_send(Conn#connection.pid, {text, Msg}),
    {noreply, State}.

handle_info({gun_upgrade, _Pid, Stream, _Protos, _Headers},
            S=#state{connection=C}) ->
    {noreply, S#state{connection=C#connection{stream=Stream}}};
handle_info({gun_ws, _ConnPid, _Ref, {text, Body}}, State) ->
    Msg = decode_payload(Body),
    {noreply, handle_message(Msg#payload.op, Msg, State)}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% helper methods
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

generate_os_string() ->
    {Family, OS} = os:type(),
    binary:list_to_bin(lists:flatten(io_lib:format("~p/~p", [Family, OS]))).

decode_payload(Msg) ->
    #{<<"op">> := Op,
      <<"d">> := D,
      <<"s">> := S,
      <<"t">> := T} = jiffy:decode(Msg, [return_maps]),
    #payload{op = Op, d = D, s = S, t = T}.

handle_message(_Code, _Msg, S) -> S.
