%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Concurrent Programming %%%
%%%            in          %%%
%%%          Erlang        %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(lec34).
-compile(export_all).

%% Sequential functions %%

fact(0) ->
		1;
fact(N) when N > 0 ->
		N * fact(N-1).

insert(X, []) ->
		[X];
insert(X, [X | Xs]) ->
		[X | Xs];
insert(X, [Y | Xs]) when X /= Y ->
		[Y | insert(X, Xs)].

reverse_append([X | Xs], Ys) ->
		reverse_append(Xs, [X | Ys]);
reverse_append([], Ys) ->
		Ys.

%% Throwing and handling exceptions %%

lookup_thrown(X, [X | Rest], Others) ->
		{X, Rest, Others};
lookup_thrown(X, [Y | Rest], Others) ->
		lookup_thrown(X, Rest, [Y | Others]);
lookup_thrown(X, [], Others) ->
		throw({lookup_not_found, X, Others}).

lookup(X, List) ->
		try lookup_thrown(X, List, []) of
				{Y, Rest, Others} -> {Y, reverse_append(Others, Rest)}
		catch
				{lookup_not_found, X} ->
						throw({not_found, X, List})
		end.

%% Spawning processes %%

% > self().
% > exit(self()).
% > self().

% > self() ! hello.
% > self() ! self() ! twice.
% > flush().

dolphin() ->
		receive
				do_a_flip ->
						io:format("The dolphin gracefully flips through the air.~n"),
				    dolphin();
				fish ->
						io:format("So long and thanks for all the fish!~n");
				_ ->
						io:format("The dolphin stares in confusion...~n"),
						dolphin()
		end.

%% Sending messages %%

fridge(Inventory) ->
		receive
				{From, look} ->
						From ! {self(), {ok, Inventory}},
						fridge(Inventory);
				{From, {store, Food}} ->
						From ! {self(), ok},
						fridge([Food | Inventory]);
				{From, {take, Food}} ->
						try lookup(Food, Inventory) of
								{Food, Remaining} ->
										From ! {self(), {ok, Food}},
										fridge(Remaining)
						catch 
								{lookup_not_found, Food, Inventory}->
										From ! {self(), {not_found, Food}},
										fridge(Inventory)
						end;
				terminate ->
						ok
		end.

look(Pid) ->
		Pid ! {self(), look},
		receive
				{Pid, Msg} -> Msg
		end.

asynch(Pid, Message) ->
		Pid ! {self(), Message}.

synch(Pid, Message) ->
		Pid ! {self(), Message},
		receive
				{Pid, Response} -> Response
		end.

store(Pid, Food) -> synch(Pid, {store, Food}).

take(Pid, Food) -> synch(Pid, {take, Food}).


%% Concurrent Finite State Machines %%

% Dog states

bark(Name) ->
		io:format("~s says: Bark! Bark!~n", [Name]),
		receive
				{From, pet} ->
						From ! {self(), {ok, wag}},
						wag_tail(Name);
				{From, _} ->
						io:format("~s is confused...~n", [Name]),
						From ! {self(), {err, bark}},
						bark(Name)
		after 2000 ->
						bark(Name)
		end.

wag_tail(Name) ->
		io:format("~s wags its tail.~n", [Name]),
		receive
				{From, pet} ->
						From ! {self(), {ok, sit}},
						sit(Name);
				{From, _} ->
						io:format("~s is confused...~n", [Name]),
						From ! {err, wag},
						wag_tail(Name)
		after 10000 ->
						bark(Name)
		end.

sit(Name) ->
		io:format("~s sits patiently. Good doggy!~n", [Name]),
		receive
				{From, pet} ->
						From ! {self(), {ok, wag}},
						wag_tail(Name);
				{From, {toss, Toy}} ->
						fetch(Name, Toy, From);
				{From, squirrel} ->
						From ! {self(), {ok, bark}},
						bark(Name);
				{From, _} ->
						io:format("~s is confused~n", [Name]),
						From ! {self(), {err, sit}},
						sit(Name)
		end.

fetch(Name, Toy, From) ->
		io:format("~s chases after the ~s!~n", [Name, Toy]),
		receive
				% The dog is on its own... don't wait for any messages
		after 5000 ->
						io:format("~s brings back the ~s.~n", [Name, Toy]),
						From ! {self(), {ok, {fetch, Toy}}},
						wag_tail(Name)
		end.

% Human states

calm(Name, Dog, Toy) ->
		io:format("~s pets the dog.~n", [Name]),
		Dog ! {self(), pet},
		receive
				{Dog, {_, bark}} ->
						calm(Name, Dog, Toy);
				{Dog, {_, wag}} ->
						calm(Name, Dog, Toy);
				{Dog, {ok, sit}} ->
						play(Name, Dog, Toy);
				{Dog, _} ->
						io:format("~s doesn't know what happened...~n", [Name]),
						calm(Name, Dog, Toy)
		end.

play(Name, Dog, Toy) ->
		io:format("~s throws a ~s!~n", [Name, Toy]),
		Dog ! {self(), {toss, Toy}},
		receive
				{Dog, {ok, {fetch, Toy}}} ->
						calm(Name, Dog, Toy);
				{Dog, _} ->
						io:format("~s doesn't know what happened...~n", [Name]),
						calm(Name, Dog, Toy)
		end.
