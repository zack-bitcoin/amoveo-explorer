-module(jobs).
-behaviour(gen_server).
-export([start_link/0,code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,terminate/2,
         new/7, adjust/4, team_adjust/5, buy/4, 
         receive_salary/2,

         boss/1]).

-define(LOC, "jobs.db").

-record(job, {id, worker, boss, salary, balance, value, height}).

%-record(db, {by_id = dict:new()}).

boss(Job = #job{boss = Boss}) -> Boss.


init(ok) -> 
    process_flag(trap_exit, true),
    X = db:read(?LOC),
    Y = if
            (X == "") -> dict:new();
            true -> X
        end,
    {ok, Y}.
%init(ok) -> {ok, []}.
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, ok, []).
code_change(_OldVsn, State, _Extra) -> {ok, State}.
terminate(_, X) -> 
    db:save(?LOC, X),
    io:format("jobs died!"), 
    ok.
handle_info(_, X) -> {noreply, X}.
handle_cast({new, ID, Worker, Boss, Salary, 
             Balance, Value, Height}, X) -> 
    Job = #job{id = ID, worker = Worker, boss = Boss,
              salary = Salary, balance = Balance,
              value = Value, height = Height},
    X2 = dict:store(ID, Job, X),
    {noreply, X2};
handle_cast({adjust, ID, NewBalance, NewValue, 
             Height}, X) -> 
    X2 = case dict:find(ID, X) of
             error -> X;
             {ok, Job} ->
                 Job2 = Job#job{
                          balance = NewBalance, 
                          value = NewValue, 
                          height = Height},
                 dict:write(ID, Job2, X)
         end,
    {noreply, X2};
handle_cast({team_adjust, ID, NewBalance, NewValue,
            NewSalary, Height}, X) -> 
    X2 = case dict:find(ID, X) of
             error -> X;
             {ok, Job} ->
                 Job2 = Job#job{
                          balance = NewBalance, 
                          value = NewValue, 
                          height = Height,
                          salary = NewSalary},
                 dict:write(ID, Job2, X)
         end,
    {noreply, X2};
handle_cast({receive_salary, ID, Height}, X) -> 
    X2 = case dict:find(ID, X) of
             error -> X;
             {ok, Job} ->
                 Job2 = Job#job{
                          height = Height},
                 dict:write(ID, Job2, X)
         end,
    {noreply, X2};
handle_cast(_, X) -> {noreply, X}.
handle_call({read, ID}, _From, X) -> 
    J = case dict:find(ID, X) of
            error -> empty;
            {ok, Job} -> Job
        end,
    {reply, J, X};
handle_call(_, _From, X) -> {reply, X, X}.


read(ID) -> gen_server:call(?MODULE, {read, ID}).

new(ID, Worker, Boss, Salary, 
    Balance, Value, Height) ->
    gen_server:cast(
      ?MODULE, {new, ID, Worker, Boss, Salary, 
                Balance, Value, Height}).
adjust(ID, NewBalance, NewValue, Height) ->
    gen_server:cast(
      ?MODULE, {adjust, ID, NewBalance, 
                NewValue, Height}).
team_adjust(ID, NewBalance, NewValue, 
            NewSalary, Height) ->
    gen_server:cast(
      ?MODULE, {team_adjust, ID, NewBalance, 
                NewValue, NewSalary, Height}).
buy(ID, NewBoss, NewBalance, Height) ->
    gen_server:cast(
      ?MODULE, {buy, ID, NewBoss, 
                NewBalance, Height}).
receive_salary(ID, Height) ->
    gen_server:cast(
      ?MODULE, {receive_salary, ID, Height}).
