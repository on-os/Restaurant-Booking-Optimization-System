:- use_module([library(lists)]).
:- use_module([library(clpfd)]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%% TASK 1 --> The DCG Aspect %%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%% The definition of menu_type/1 is given below: %%%%%%%%%%%%%%%
% menu_type/1 will succeed when the text parsed is one of the two recognized 
% menu types in the restaurant
% This "Menu" that's being extracted from here would be useful later on.
menu_type(Menu) --> [Menu], {member(Menu, [standard, theatre])}.


%%%%%%%%%%% noun will succeed when the text parsed denotes a noun %%%%%%%%%%%%
noun --> [reservation].
noun --> [party].
noun --> [place].
noun --> [space].

%% object will succeed when the text parsed denotes an object in this context %
object --> [table].
object --> [a, table].


%%%% subject will succeed when the text parsed represents a pronoun in the %%%
% place of a subject 
subject --> [we].
subject --> [i].


%%%% verb will succeed when the text parsed means a verb i.e a doing word %%%%
verb --> [reserve].
verb --> [can].
verb --> [book].
verb --> [have].
verb --> [would].
verb --> [like].
verb --> [allow].


%%%%%%%%% adverb will succeed when the text parsed denotes an adverb %%%%%%%%%
adverb --> [please].
adverb --> [preferably].


%%%% preposition will succeed when the text parsed denotes a preposition %%%%%
preposition --> [for].
preposition --> [at].
preposition --> [on].
preposition --> [in].
preposition --> [next].
preposition --> [of].
preposition --> [beside].
preposition --> [behind].
preposition --> [between].
preposition --> [by].


%%%% det will succeed when the text parsed represents an article determiner %%%
det --> [a].
det --> [an].
det --> [the].


%%%% verb_phrase will succeed when the text parsed represents a verb phrase 
% within scope of the restaurant 
verb_phrase --> [would, like].
verb_phrase --> [can, i, book].
verb_phrase --> [can, we, book].
verb_phrase --> [can, we, have].
verb_phrase --> [can, i, have].
verb_phrase --> [reserve, us].
verb_phrase --> [reserve, for, us].
verb_phrase --> [make, a, reservation, for, us].
verb_phrase --> [make, a, reservation].


%%%% noun_phrase will succeed when the text parsed represents a noun phrase %%%
noun_phrase --> noun.
noun_phrase --> det, noun.


%%%%%% number/1 represents the integer for the number of people that can book a
% spot 
% I assume the minimum and maximum to be 2 and 10 respectively.
number(Number) --> [Number], {integer(Number), Number >=2, Number =<10}.


%%%% preposition_phrase/1 will succeed when the text parsed include an integer
% representing the number of people to book for in this context
% The number that's being extracted here would be useful later on.
preposition_phrase(Number) --> [for], number(Number), [people].
preposition_phrase(Number) --> [for], number(Number).
preposition_phrase(Number) --> [for], number(Number), [persons].
preposition_phrase(Number) --> [for, a, party, of], number(Number), [people].
preposition_phrase(Number) --> [for, a, party, of], number(Number).
preposition_phrase(Number) --> [for, a, party, of], number(Number), [persons].
preposition_phrase(Number) --> number(Number), [of, us].
preposition_phrase(Number) --> number(Number), [of, us, in].
preposition_phrase(Number) --> number(Number), [people].
preposition_phrase(Number) --> number(Number), [persons].


% preposition_phrase/1 will succeed when the text parsed represent the acceptable 
% menu phrase that can exist in the restaurant
menu_phrase(Menu) --> [for], det, menu_type(Menu), [menu].
menu_phrase(Menu) --> [for], det, menu_type(Menu), [menu], adverb.
menu_phrase(Menu) --> [for], menu_type(Menu), [menu].
menu_phrase(Menu) --> [for], menu_type(Menu), [menu], adverb.

% in case the menu is not specified by the user 
% since it is allowed to be left blank atimes
menu_phrase(_Menu) --> [].	


%%%%%%%%%%%% date/1 will succeed when the text parsed is a valid date  %%%%%%%%%%%
% My assumption is that month is usually entered in full 
% e.g always january but not jan, etc

% e.g 6 January 
date([Day, Month]) --> day(Day), month(Month). 		

% e.g January 6
date([Day, Month]) --> month(Month), day(Day).		

% e.g 6th January
date([Day, Month]) --> day(Day), day_suffix(Day), month(Month).	

% e.g January 6th
date([Day, Month]) --> month(Month), day(Day), day_suffix(Day).		

% e.g 6/1
date([Day, Month]) --> day(Day), [/], month(Month). 		

% e.g 6 of January
date([Day, Month]) -->day(Day), [of], month(Month).		

% e.g 6th of January
date([Day, Month]) -->day(Day), day_suffix(Day), [of], month(Month). 


%%%%%% day_suffix/1 will succeed when the text parsed is an ordinal indicator for
% the specified date
day_suffix(Day) --> [th], {Day >= 4, Day =\= 21, Day =\= 22, Day =\= 23, Day =\= 31}.
day_suffix(Day) --> [st], {Day =:= 1}.
day_suffix(Day) --> [st], {Day =:= 21}.
day_suffix(Day) --> [st], {Day =:= 31}.
day_suffix(Day) --> [nd], {Day =:= 2}.
day_suffix(Day) --> [nd], {Day =:= 22}.
day_suffix(Day) --> [rd], {Day =:= 3}.
day_suffix(Day) --> [rd], {Day =:= 23}.


%% month/1 will succeed when the text parsed is a valid month representation %%
% Defining the month/1 constraint that a valid month should be an integer value 
% between 1 and 12
month(Month) --> [Month], {integer(Month), Month>=1, Month<13}.

% Defining each month from january to december, allowing for all possibilities
% for each month that we can have
month(1) --> [january].
month(1) --> [01].
month(1) --> [1].
month(2) --> [february].
month(2) --> [02].
month(2) --> [2].
month(3) --> [march].
month(3) --> [03].
month(3) --> [3].
month(4) --> [april].
month(4) --> [04].
month(4) --> [4].
month(5) --> [may].
month(5) --> [05].
month(5) --> [5].
month(6) --> [june].
month(6) --> [06].
month(6) --> [6].
month(7) --> [july].
month(7) --> [07].
month(7) --> [7].
month(8) --> [august].
month(8) --> [08].
month(8) --> [8].
month(9) --> [september].
month(9) --> [09].
month(9) --> [9].
month(10) --> [october].
month(10) --> [10].
month(11) --> [november].
month(11) --> [11].
month(12) --> [december].
month(12) --> [12].


%%%%% day/1 will succeed when the text parsed is a valid day representation %%%%%
% my assumption is that day in date  is usually entered in its integer equivalent 
% e.g always 1 or 1st but not first, etc
% a normal day in date should range between 1 and 31
day(Day) --> [Day], {integer(Day), Day>=1, Day<32}.


%%%% date_phrase/1 will succeed when the text parsed include a valid date %%%%%%
% Describing the possible date phrases that could exist:
date_phrase([Day, Month]) --> [on], date([Day, Month]).
date_phrase([Day, Month]) --> [on, the], date([Day, Month]).
date_phrase([Day, Month]) --> [by], date([Day, Month]).


%%%%%%%%%% time/1 will succeed when the text parsed is a valid time %%%%%%%%%%%
time([Hour, Minute]) --> hour(Hour), [:], minute(Minute). 
time([Hour, 0]) --> hour(Hour), [pm].
time([Hour, 0]) --> hour(Hour), [oclock].
time([Hour, 0]) --> hour(Hour).


%%%%%%%%%% hour/1 will succeed when the text parsed is a valid hour %%%%%%%%%%
hour(Hour) --> 
[InputHour], {integer(InputHour), InputHour >= 0, InputHour =< 23, 
Hour is ((InputHour-1) mod 12 )+ 1 }.
% I have used mod 12 above so that if time of the form 20:00 or 23:00 is gotten, 
% it can be converted to 8 or 11 respectively, etc and this will make life easier 
% for the restaurant attendees.


%%%%% minute/1 will succeed when the text parsed is a valid minute integer %%%%
minute(Minute) --> [Minute], {integer(Minute), Minute>=0, Minute=<59}. 


%%%% time_phrase/1 will succeed when the text parsed include a valid time %%%%
% Describing the possible time phrases that could exist:
time_phrase([Hour, Minute]) --> [at], time([Hour, Minute]). 
time_phrase([Hour, Minute]) --> [by], time([Hour, Minute]). 

% in case time is not specified by the user,
% since it is allowed to sometimes be left blank
time_phrase([Hour, Minute]) --> [], {Hour = _, Minute = _}.


%%%%%%%%%%%%%%%%%%%%%% Sentence Construction and Design %%%%%%%%%%%%%%%%%%%%%%

% e.g Table for 2 at 20:00 on 18 March
order_message([Number, Date, Menu, Time]) -->
object, preposition_phrase(Number), 
menu_phrase(Menu), time_phrase(Time),
date_phrase(Date).

% e.g Please can we have a table for 3 for the theatre menu on March 18th?
order_message([Number, Date, Menu, Time]) -->
adverb, verb_phrase, object,
preposition_phrase(Number), menu_phrase(Menu),
date_phrase(Date), time_phrase(Time).

% e.g Have a table for 6 for the standard menu on October 10th
order_message([Number, Date, Menu, Time]) -->
verb, object,
preposition_phrase(Number), menu_phrase(Menu),
date_phrase(Date), time_phrase(Time).

% e.g Have a table on april 16 for a party of 2
order_message([Number, Date, Menu, Time]) -->
verb, object,
date_phrase(Date), preposition_phrase(Number), 
menu_phrase(Menu), time_phrase(Time).

% e.g We would like a table for 5 preferably at 8pm on 18/03.
order_message([Number, Date, Menu, Time]) -->
subject, verb_phrase, object, preposition_phrase(Number),
adverb, time_phrase(Time), date_phrase(Date), 
menu_phrase(Menu).

% e.g Can I book a table at 9pm for 2 people on the 19th of March for 
% the standard menu please?
order_message([Number, Date, Menu, Time]) -->
verb_phrase, object, time_phrase(Time),
preposition_phrase(Number), date_phrase(Date),
menu_phrase(Menu).

% e.g Reserve us a table on March 18 for a party of 4 for the standard menu.
order_message([Number, Date, Menu, Time]) -->
verb_phrase, object, date_phrase(Date),
preposition_phrase(Number), menu_phrase(Menu),
time_phrase(Time).

% e.g A table on March 18 for a party of 4.
order_message([Number, Date, Menu, Time]) -->
object, date_phrase(Date),
preposition_phrase(Number), menu_phrase(Menu),
time_phrase(Time).

% e.g A table on March 18 for 4.
order_message([Number, Date, Menu, Time]) -->
object, date_phrase(Date),
preposition_phrase(Number), menu_phrase(Menu),
time_phrase(Time).


% e.g 9 people on 18th of March.
order_message([Number, Date, Menu, Time]) -->
preposition_phrase(Number), date_phrase(Date),
menu_phrase(Menu), time_phrase(Time).

% e.g Book 6 of us in on 18 March at 20:00.
order_message([Number, Date, Menu, Time]) -->
verb, preposition_phrase(Number), date_phrase(Date),
time_phrase(Time), menu_phrase(Menu).

% e.g Reservation for 7 on March 18 preferably for standard menu at 7 o'clock.
order_message([Number, Date, Menu, Time]) -->
noun, preposition_phrase(Number), date_phrase(Date),
adverb, menu_phrase(Menu), time_phrase(Time).

% e.g We would like a table for 3 at 10pm on june 8th.
order_message([Number, Date, Menu, Time]) -->
subject, verb_phrase, object, preposition_phrase(Number),
time_phrase(Time), date_phrase(Date), 
menu_phrase(Menu).	


%%%%%%%%%% Definition of test_input_#/1 and test_output_#/1 %%%%%%%%%%
% test_input_# succeeds when its argument is a list of valid input data
% test_outpux_# succeeds when its argument represents the output of the 
% test_input_#
test_input_1([table,for,2,at,20,':',00,on,18,march]).
test_output_1([2,[18,3],_,[8,0]]).

test_input_2([please,can,we,have,a,table,for,3,for,the,theatre,menu,on,march,18,th]).
test_output_2([3,[18,3],theatre,[_,_]]).

test_input_3([we,would,like,a,table,for,5,preferably,at,8,pm,on,18,'/',03]).
test_output_3([5, [18, 3], _, [8, 0]]).

test_input_4([can,i,book,a,table,at,9,pm,for,2,people,on,the,18,th,of,march,for,the,standard,menu,please]).
test_output_4([2, [18, 3], standard, [9, 0]]).

test_input_5([reserve,us,a,table,on,march,18,for,a,party,of,4,for,the,standard,menu]).
test_output_5([4, [18, 3], standard, [_, _]]).

test_input_6([9,people,on,18,th,of,march]).
test_output_6([9, [18, 3], _, [_, _]]).

test_input_7([book,6,of,us,in,on,18,march,at,20,':',00]).
test_output_7([6, [18, 3], _, [8, 0]]).

test_input_8([reservation,for,7,on,march,18,preferably,for,standard,menu,at,7,oclock]).
test_output_8([7, [18, 3], standard, [7, 0]]).

test_input_9([a,table,on,march,18,for,a,party,of,4]).
test_output_9([4, [18, 3], _, [_, _]]).

test_input_10([table,for,6,by,7,':',00,on,2,nd,august]).
test_output_10([6, [2, 8], _, [7, 0]]).


%%%%%%%%%%%%%%%%%%%%%% Definition of test_dcg/2 %%%%%%%%%%%%%%%%%%%%%%%
% test_dcg/2 succeeds if its first argument is an input phrase in the given
% format, and its second argument is whatever DCG returns to my program.

test_dcg(Input1,Output1) :-
         test_input_1(Input1), 
         test_output_1(Output1),
		 phrase(order_message(Output1), Input1, []).

test_dcg(Input2,Output2) :-
         test_input_2(Input2), 
         test_output_2(Output2),
		 phrase(order_message(Output2), Input2, []).

test_dcg(Input3,Output3) :-
         test_input_3(Input3), 
         test_output_3(Output3),
		 phrase(order_message(Output3), Input3, []).

test_dcg(Input4,Output4) :-
         test_input_4(Input4), 
         test_output_4(Output4),
		 phrase(order_message(Output4), Input4, []).

test_dcg(Input5,Output5) :-
         test_input_5(Input5), 
         test_output_5(Output5),
		 phrase(order_message(Output5), Input5, []).		 

test_dcg(Input6,Output6) :-
         test_input_6(Input6), 
         test_output_6(Output6),
		 phrase(order_message(Output6), Input6, []).

test_dcg(Input7,Output7) :-
         test_input_7(Input7), 
         test_output_7(Output7),
		 phrase(order_message(Output7), Input7, []).

test_dcg(Input8,Output8) :-
         test_input_8(Input8), 
         test_output_8(Output8),
		 phrase(order_message(Output8), Input8, []).

test_dcg(Input9,Output9) :-
         test_input_9(Input9), 
         test_output_9(Output9),
		 phrase(order_message(Output9), Input9, []).
		 

test_dcg(Input10,Output10) :-
         test_input_10(Input10), 
         test_output_10(Output10),
		 phrase(order_message(Output10), Input10, []).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%% TASK 2 --> The CLP Aspect %%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%% The definition of menu_time/3 is given below: %%%%%%%%%%%%%%%
% menu_time/3 succeeds when its first, second and third argument is an integer 
% that represents the key of the menu type, menu name, and the time allowed in 
% minutes respectively.
menu_time(1, theatre, 60).
menu_time(2, standard, 120).	

% the “standard" menu type is assumed if the menu type is not specified by 
% the user.
menu_time(3, notSpecified, 120).


%%%%%%%%%%%%% The definition of time_user_start/2 is given below: %%%%%%%%%%%%%%
% time_user_start/2 will succeed when its 1st, and 2nd arguments are assigned
% integer value, and the category of start time respectively.
time_user_start(1, fixed).
time_user_start(2, preferred).
time_user_start(3, noStartTime).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%% TASK 3 --> The Test Cases %%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% from the pre-processed data:
% phrase(order_message(BookingInfo), [table,for,2,at,20,':',00,on,22,nd,march], []). 														
% phrase(order_message(BookingInfo), [please,can,we,have,a,table,for,3,for,the,theatre,menu,on,march,31,st], []).
% phrase(order_message(BookingInfo), [we,would,like,a,table,for,5,preferably,at,8,pm,on,18,'/',03], []).
% phrase(order_message(BookingInfo), [can,i,book,a,table,at,9,pm,for,2,people,on,the,18,th,of,march,for,the,standard,menu,please], []).
% phrase(order_message(BookingInfo), [reserve,us,a,table,on,march,18,for,a,party,of,4,for,the,standard,menu], []).	
% phrase(order_message(BookingInfo), [9,people,on,18,th,of,march], []).												
% phrase(order_message(BookingInfo), [book,6,of,us,in,on,18,march,at,20,':',00], []).								
% phrase(order_message(BookingInfo), [reservation,for,7,on,march,18,preferably,for,standard,menu,at,7,oclock], []).	

% extra data for testing purpose:
% phrase(order_message(BookingInfo), [table,for,4,at,21,':',00,on,20,january], []).									
% phrase(order_message(BookingInfo), [please,can,i,have,a,table,for,2,for,the,standard,menu,on,3,rd,february], []).	
% phrase(order_message(BookingInfo), [we,would,like,a,table,for,3,at,10,pm,on,june,3,rd], []).						
% phrase(order_message(BookingInfo), [table,for,6,by,7,':',00,on,2,nd,august], []).									
% phrase(order_message(BookingInfo), [a,table,on,march,18,for,a,party,of,4], []).									
% phrase(order_message(BookingInfo), [a,table,on,march,18,for,4], []).												
% phrase(order_message(BookingInfo), [have,a,table,on,march,18,for,a,party,of,10], []).								
% phrase(order_message(BookingInfo), [have,a,table,for,6,for,the,standard,menu,on,october,10,th], []).				