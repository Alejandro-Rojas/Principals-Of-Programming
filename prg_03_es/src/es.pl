% Programming Assignment 03 An Expert System
% Alejandro Rojas, Eesha Patel

begin :-
  init,
  (read(yes) ->
  reset_answers,
  find_videogame(VideoGame),
  game(VideoGame), nl,
  confirm
  ;
  write('Bye')).
begin :-
  write('Please try again by typing begin.'), nl.

init :-
  write('Welcome to this ES about Video Games!'), nl,
  write('I am going to ask about Video Games Genres.'), nl,
  write('Please answer yes. or no.'), nl,
  write('Ready?'), nl.

reset_answers :-
retractall(progress(_, _)).
reset_answers.

find_videogame(VideoGame) :-
  videogame(VideoGame), !.

confirm :-
  write('Did I get it right?'), nl,
  (read(yes) ->
  write('Glad I could help!')
  ;
  write('Not my fault! My designer did not give me enough information about video games.'), nl,  
  write('Feel free to start again by typing begin.'), nl).

  % Saves user input
ask(Question, Answer, Options) :-
  question(Question),
  read(Response),
  asserta(progress(Question, Response)),
  Response = Answer.

:- dynamic(progress/2).

% Rules about videogame
videogame(apexlegends) :-
  battleroyal(yes),
  multiplayer(yes).


videogame(mortalkombat) :-
platformgame(yes),
  fighting(yes).

videogame(halo) :-
  strategy(yes),
  fps(yes).

videogame(fornite) :-
  battleroyal(yes),
  survival(yes),
  multiplayer(yes).

videogame(mario) :-
  platformgame(yes),
  rpg(yes).


videogame(thelastofus) :-
  survivalhorror(yes),
  rpg(yes).

videogame(forzahorizon) :-
  openworld(yes),
  racing(yes).

videogame(grandtourismo) :-
  racing(yes),
  multiplayer(yes).

videogame(dota) :-
  multiplayer(yes),
  moab(yes).

videogame(fallout) :-
  rpg(yes),
  survival(yes).

videogame(destiny) :-
  rpg(yes),
  moab(yes).

videogame(fornite) :-
  rpg(yes),
  battleroyal(yes).

videogame(2k2022) :-
  sport(yes),
  multiplayer(yes),
  rpg(yes).

videogame(asssinscreed) :-
  rpg(yes).

videogame(zelda) :-
  platformgame(yes),
  rpg(yes).

videogame(fifa) :-
  sport(yes).

% knowledge Base
game(apexlegends) :-
  write('Apex Legends'), nl.
  
game(mortalkombat) :-
  write('Mortal Kombat'), nl.
  

game(mario) :-
  write('Mario'), nl.
  
game(thelastofus) :-
  write('The Last of Us'), nl.
  

game(forzahorizon) :-
  write('Forza Horizon'), nl.
  
game(grandtourismo) :-
  write('Grand Tourismo'), nl.
 

game(dota) :-
  write('Dota'), nl.
  
game(fallout) :-
  write('Fallout'), nl.
  

game(destiny) :-
  write('Destiny'), nl.
 

game(fortnite) :-
  write('Fortnite'), nl.

game(2k2022) :-
  write('2k2022'), nl.
 

game(assasingcreed) :-
  write('Assins Creed'), nl.
  

game(zelda) :-
  write('Zelda'), nl.
 

game(fifa) :-
  write('Fifa'), nl.

game(halo) :-
  write('Halo'), nl.

% Questions about the knowledge base
question(fps) :-
  write('Do you like fps games?'), nl.

question(fighting) :-
  write('Do you enjoy Fighting games?'), nl.

question(rpg) :-
  write('Do you like rpg games?'), nl.

question(moab) :-
  write('Do you like moab games?'), nl.

question(battleroyal) :-
  write('Do you like battleroyal games?'), nl.

question(survival) :-
  write('Do you like survival games?'), nl.

question(platformgame) :-
  write('Do you enjoy platformgames?'), nl.

question(survivalhorror) :-
  write('Do you enjoy survivalhorror games?'), nl.

question(racing) :-
  write('Do you like cars and racing?'), nl.

question(sport) :-
  write('Do you enjoy sport games?'), nl.

question(multiplayer) :-
  write('Do you enjoy multiplayer games?'), nl.

question(strategy) :-
  write('Do you enjoy stradegy games?'), nl.

question(openworld) :-
  write('Do you enjoy open world games?'), nl.



option(yes).
option(no).


% knowledge base 2
fps(Answer) :-
  progress(fps, Answer).
fps(Answer) :-
  \+ progress(fps, _),
  ask(fps, Answer, [yes, no]).

multiplayer(Answer) :-
  progress(multiplayer, Answer).
multiplayer(Answer) :-
  \+ progress(multiplayer, _),
  ask(multiplayer, Answer, [yes, no]).

survival(Answer) :-
  progress(survival, Answer).
survival(Answer) :-
  \+ progress(survival, _),
  ask(survival, Answer, [yes, no]).


openworld(Answer) :-
  progress(openworld, Answer).
openworld(Answer) :-
  \+ progress(openworld, _),
  ask(openworld, Answer, [yes, no]).

strategy(Answer) :-
  progress(strategy, Answer).
strategy(Answer) :-
  \+ progress(strategy, _),
  ask(strategy, Answer, [yes, no]).


battleroyal(Answer) :-
  progress(battleroyal, Answer).
battleroyal(Answer) :-
  \+ progress(battleroyal, _),
  ask(battleroyal, Answer, [yes, no]).

fighting(Answer) :-
  progress(fighting, Answer).
fighting(Answer) :-
  \+ progress(fighting, _),
  ask(fighting, Answer, [yes, no]).

moab(Answer) :-
  progress(moab, Answer).
moab(Answer) :-
  \+ progress(moab, _),
  ask(moab, Answer, [yes, no]).

platformgame(Answer) :-
  progress(platformgame, Answer).
platformgame(Answer) :-
  \+ progress(platformgame, _),
  ask(platformgame, Answer, [yes, no]).

survivalhorror(Answer) :-
  progress(survivalhorror, Answer).
survivalhorror(Answer) :-
  \+ progress(survivalhorror, _),
  ask(survivalhorror, Answer, [yes, no]).

racing(Answer) :-
  progress(racing, Answer).
racing(Answer) :-
  \+ progress(racing, _),
  ask(racing, Answer, [yes, no]).

sport(Answer) :-
  progress(sport, Answer).
sport(Answer) :-
  \+ progress(sport, _),
  ask(sport, Answer, [yes, no]).

rpg(Answer) :-
  progress(rpg, Answer).
rpg(Answer) :-
  \+ progress(rpg, _),
  ask(rpg, Answer, [yes, no]).



