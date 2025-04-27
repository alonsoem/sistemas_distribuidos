-module(worker).
-import(time,[zero/0,inc/0]).
-export([start/5, stop/1, peers/2]).

start(Name, Logger, Seed, Sleep, Jitter) ->
    spawn_link(fun() -> init(Name, Logger, Seed, Sleep, Jitter) end).

stop(Worker) ->
    Worker ! stop.

init(Name, Log, Seed, Sleep, Jitter) ->
    random:seed(Seed, Seed, Seed),
    receive
        {peers, Peers} ->
            WorkerTime = time:zero(),
            loop(Name, Log, Peers, Sleep, Jitter,WorkerTime);
        stop ->
            ok
    end.

peers(Wrk, Peers) ->
    Wrk ! {peers, Peers}.

loop(Name, Log, Peers, Sleep, Jitter,WorkerTime)->
    Wait = random:uniform(Sleep),
    receive
        {msg, Time, Msg} ->
            Log ! {log, Name, Time, {received, Msg}},
            NewWorkerTime=time:merge(WorkerTime,Time),
            loop(Name, Log, Peers, Sleep, Jitter,NewWorkerTime);
        stop ->
            ok;
        Error ->
            Log ! {log, Name, time, {error, Error}}
    after Wait ->
            Selected = select(Peers),
            Message = {hello, random:uniform(100)},
            NewWorkerTime=time:inc(Name,WorkerTime),
            Selected ! {msg, NewWorkerTime, Message},
            jitter(Jitter),
            Log ! {log, Name, WorkerTime, {sending, Message}},
            
            loop(Name, Log, Peers, Sleep, Jitter,NewWorkerTime)
    end.

select(Peers) ->
    lists:nth(random:uniform(length(Peers)), Peers).

jitter(0) -> ok;
jitter(Jitter) -> timer:sleep(random:uniform(Jitter)).
