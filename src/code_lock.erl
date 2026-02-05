-module(code_lock).
-moduledoc "A module that allows to manage a key lock".
-behaviour(gen_statem).

-define(NAME, code_lock).

-export([start_link/2]).
-export([button/1]).
-export([set_lock_button/1]).
-export([change_code/1]).
-export([init/1]). 
-export([callback_mode/0]). 
-export([terminate/3]).
-export([handle_event/4]).

-spec start_link(list(integer()), integer()) -> gen_statem:start_ret().
start_link(Code, LockButton) ->
    %% Spawns and links to a new process (gen_statem), Code -- correct unlock code (list of digits)
    gen_statem:start_link({local, ?NAME}, ?MODULE, {Code, LockButton}, []).

stop() ->
    gen_statem:stop(?NAME).

%% Event: onButtonPress
-spec button(integer()) -> ok.
button(Button) ->
    gen_statem:cast(?NAME, {button, Button}).

-spec change_code(list(integer())) -> ok.
change_code(Code) ->
    gen_statem:cast(?NAME, {change_code, Code}).

set_lock_button(LockButton) ->
    gen_statem:call(?NAME, {set_lock_button, LockButton}).

-spec init(list(integer())) -> gen_statem:init_result(gen_statem:state()).
init({Code, LockButton}) ->
    process_flag(trap_exit, true),
    Data = #{code => Code, length => length(Code), buttons => [], attempts => 0},
    {ok, {locked, LockButton}, Data}.

%% Selects the CallbackMode for the callback module
callback_mode() ->
    [handle_event_function,state_enter].

%% Locked State
handle_event(enter, {locked, _}, _OldState, Data) ->
    {keep_state, Data#{buttons := []}};

handle_event(state_timeout, button, {locked, _}, Data) ->
    {keep_state, Data#{buttons := []}};

handle_event(
    cast, {button,Button}, {locked, LockButton},
    #{code := Code, length := Length, buttons := Buttons, attempts := Attempts} = Data) ->
    NewButtons =
        if
            length(Buttons) < Length -> Buttons;
            true -> tl(Buttons)
        end ++ [Button],
    if
        % Correct code
        NewButtons =:= Code -> do_unlock(),
            {next_state, {open, LockButton}, Data#{buttons := []}}; 
        % if attempts are used up
        length(NewButtons) == Length -> 
            case Attempts of
                2 -> do_suspend(),
                    {next_state, {suspended, LockButton}, Data#{buttons := [], attempts := 0}, {state_timeout, 10000, lock}};
                _ -> do_incorrect(),
                    {keep_state, Data#{buttons := [], attempts := Attempts + 1}}
            end;
        % Invalid code or incomplete
        true ->
            {keep_state, Data#{buttons := NewButtons}, 
            [{state_timeout, 30000, button}]}
    end;

handle_event(state_timeout, lock, {locked,_LockButton}, Data) ->
    {keep_state, Data};

%% Unlocked State
handle_event(enter, {open,_}, {_OldState, _}, _Data) ->
    {keep_state_and_data, {state_timeout, 10000, lock}}; % Time in milliseconds hibernate

handle_event(cast, {button,LockButton}, {open,LockButton}, Data) ->
    do_lock(),
    {next_state, {locked, LockButton}, Data};

handle_event(cast, {button, _NonLock}, {open,LockButton}, Data) ->
    do_help_lock(),
    {next_state, {open, LockButton}, Data};

handle_event(state_timeout, lock, {open,LockButton}, Data) ->
    do_unlock_over(),
    {next_state, {locked, LockButton}, Data};

%% Suspended State
handle_event(state_timeout, lock, {suspended, LockButton}, Data) ->
    do_suspend_over(),
    {next_state, {locked, LockButton}, Data};

handle_event(cast, {button,_}, {suspended, _LockButton}, Data) ->
    do_suspended(),
    {keep_state, Data};

handle_event(enter, {suspended, _}, {_OldState, _}, _Data) ->
    {keep_state_and_data, {state_timeout, 10000, lock}};

handle_event({call, From}, {set_lock_button, NewLockButton},
     {Statename,OldLockButton}, Data) ->
    {next_state, {Statename,NewLockButton}, Data,
    [{reply, From, OldLockButton}]};

%%Change Code

handle_event(cast, {change_code, Code}, {open, _}, Data) ->
    do_code_change(),
    {keep_state,  Data#{code => Code, length => length(Code), buttons => [], attempts => 0}};

handle_event(cast, {change_code, _}, {_, _}, Data) ->
    do_incorrect_code_change(),
    {keep_state,  Data}.

%%Messages
do_lock() ->
    io:format("Locked~n", []).

do_help_lock() ->
    io:format("use lock button to lock~n", []).

do_unlock() ->
    io:format("Unlocked~n", []).

do_unlock_over() ->
    io:format("Open time limit reached, Locked~n", []).

do_suspend() ->
    io:format("Too many failed attempts, suspended~n", []).

do_suspended() ->
    io:format("try again later~n", []).

do_suspend_over() ->
    io:format("suspension over~n", []).

do_incorrect_code_change() ->
    io:format("Lock has to be opened to change code~n", []).

do_code_change() ->
    io:format("Successfully changed the code~n", []).

do_incorrect() ->
    io:format("Incorrect~n", []).

terminate(_Reason, State, _Data) ->
    State =/= locked andalso do_lock(),
    ok.