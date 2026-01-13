% Weather Prediction System 
 
:- dynamic current_temperature/1.
:- dynamic current_humidity/1.
:- dynamic current_pressure/1.
:- dynamic current_wind_speed/1.
:- dynamic current_uv_index/1.
:- dynamic current_location/1.

% Temperature Conversion

fahrenheit_to_celsius(F, C) :-number(F), C is (F - 32) * 5 / 9.

%  Input Validation Predicates

valid_temperature(T) :- number(T).
valid_humidity(H) :- number(H), H >= 0, H =< 100.
valid_pressure(P) :- number(P), P >= 800, P =< 1100.
valid_wind_speed(W) :- number(W), W >= 0.
valid_uv_index(UV) :- number(UV), UV >= 0, UV =< 15.
valid_location(Loc) :- atom(Loc).


% Temperature condition

temp_condition(T, cold) :- T < 10.
temp_condition(T, cool) :- T >= 10, T < 20.
temp_condition(T, moderate_temp) :- T >= 20, T < 30.
temp_condition(T, warm) :- T >= 30, T < 35.
temp_condition(T, hot) :- T >= 35.

% Rain likelihood based on humidity and pressure

rain_likelihood(Humidity, Pressure, rain) :- Humidity > 75, Pressure < 1000.
rain_likelihood(Humidity, Pressure, possible_rain) :- Humidity >= 50, Humidity =< 75, Pressure < 1015.
rain_likelihood(_, Pressure, no_rain) :- Pressure >= 1015.

% Wind condition

wind_condition(Wind, calm) :- Wind < 5.
wind_condition(Wind, breezy) :- Wind >= 5, Wind < 20.
wind_condition(Wind, windy) :- Wind >= 20.

% Humidity condition

humidity_condition(Humid, dry) :- Humid < 40.
humidity_condition(Humid, comfortable) :- Humid >= 40, Humid =< 60.
humidity_condition(Humid, humid) :- Humid > 60.

% UV condition

uv_condition(UV, uv_low) :- UV =< 2.
uv_condition(UV, uv_moderate) :- UV > 2, UV =< 5.
uv_condition(UV, uv_high) :- UV > 5, UV =< 7.
uv_condition(UV, uv_very_high) :- UV > 7, UV =< 10.
uv_condition(UV, uv_extreme) :- UV > 10.

% current date and time

current_date_time(DateTime) :-
    get_time(Stamp),
    format_time(atom(DateTime), '%Y-%m-%d %H:%M:%S', Stamp).

% Provide weather prediction current stored data

provide_weather_prediction :-
    current_date_time(DateTime),
    current_temperature(T),
    current_humidity(H),
    current_pressure(P),
    current_wind_speed(W),
    current_uv_index(UV),
    current_location(Loc),
    temp_condition(T, TempCond),
    rain_likelihood(H, P, RainCond),
    wind_condition(W, WindCond),
    humidity_condition(H, HumidCond),
    uv_condition(UV, UVCond),
    format("Weather prediction for ~w (Date & Time: ~w):~n", [Loc, DateTime]),nl,
    format("- Temperature: ~2f °C (~w)~n", [T, TempCond]),nl,
    format("- Rain likelihood: ~w~n", [RainCond]),nl,
    format("- Wind condition: ~w (~2f km/h)~n", [WindCond, W]),nl,
    format("- Humidity: ~w (~2f %)~n", [HumidCond, H]),nl,
    format("- UV Index: ~w (~2f)~n", [UVCond, UV]),nl,
    give_advice(RainCond, UVCond, WindCond).

%  Advices based conditions

give_advice(rain, _, _) :-write("- Advice: Take an umbrella or raincoat!....."), nl.
give_advice(possible_rain, _, _) :-write("- Advice: Carry an umbrella just in case....."), nl.
give_advice(_, uv_high, _) :-write("- Advice: Wear sunglasses, hat, and sunscreen....."), nl.
give_advice(_, uv_very_high, _) :-write("- Advice: Seek shade and apply sunscreen frequently....."), nl.
give_advice(_, uv_extreme, _) :-write("- Advice: Avoid outdoor activities during peak sun hours....."), nl.
give_advice(_, _, windy) :-write("- Advice: Secure loose items outside......"), nl.
give_advice(_, _, _) :-write("- Enjoy your day!...."), nl.

% Clear previous weather data

clear_weather_data :-
    retractall(current_temperature(_)),
    retractall(current_humidity(_)),
    retractall(current_pressure(_)),
    retractall(current_wind_speed(_)),
    retractall(current_uv_index(_)),
    retractall(current_location(_)).

%  Main program

get_weather_input :-
    clear_weather_data,
    write("Enter your location (e.g., 'paris'): "),
    read(Location),(valid_location(Location) -> asserta(current_location(Location)); write("Invalid location. Try again."), nl, get_weather_input),nl,

    write("Enter temperature unit (c for Celsius, f for Fahrenheit): "),
    read(Unit),( (Unit == c ; Unit == f) -> true; write("Invalid unit. Use 'c' or 'f'."), nl, get_weather_input ),nl,

    write("Enter temperature (numeric): "),
    read(Temp),(number(Temp) -> true ; write("Invalid temperature input."), nl, get_weather_input),
    ( Unit == f -> fahrenheit_to_celsius(Temp, TempC), asserta(current_temperature(TempC)) ; asserta(current_temperature(Temp))),nl,

    write("Enter humidity (0-100): "),
    read(Humidity),(valid_humidity(Humidity) -> asserta(current_humidity(Humidity)) ; write("Invalid humidity. Try again."), nl, get_weather_input),nl,

    write("Enter pressure in hPa (800-1100): "),
    read(Pressure),(valid_pressure(Pressure) -> asserta(current_pressure(Pressure)); write("Invalid pressure. Try again."), nl, get_weather_input),nl,

    write("Enter wind speed in km/h: "),
    read(WindSpeed),(valid_wind_speed(WindSpeed) -> asserta(current_wind_speed(WindSpeed)); write("Invalid wind speed. Try again."), nl, get_weather_input),nl,

    write("Enter UV index (0-15): "),
    read(UVIndex),(valid_uv_index(UVIndex) -> asserta(current_uv_index(UVIndex)); write("Invalid UV index. Try again."), nl, get_weather_input),nl,
    provide_weather_prediction.


% Save current weather details to file

save_weather_prediction(File) :-
    open(File, write, Stream),
    current_date_time(DateTime),
    current_location(Loc),
    current_temperature(Temp),
    current_humidity(H),
    current_pressure(P),
    current_wind_speed(W),
    current_uv_index(UV),
    temp_condition(Temp, TempCond),
    rain_likelihood(H, P, RainCond),
    wind_condition(W, WindCond),
    humidity_condition(H, HumidCond),
    uv_condition(UV, UVCond),
    format("Weather prediction for ~w (Date & Time: ~w):~n", [Loc, DateTime]),nl,
    format(Stream, "Weather prediction for ~w (Date & Time: ~w):~n", [Loc, DateTime]),nl,
    format(Stream, "- Temperature: ~2f °C (~w)~n", [Temp, TempCond]),nl,
    format(Stream, "- Rain likelihood: ~w~n", [RainCond]),nl,
    format(Stream, "- Wind condition: ~w (~2f km/h)~n", [WindCond, W]),nl,
    format(Stream, "- Humidity: ~w (~2f %)~n", [HumidCond, H]),nl,
    format(Stream, "- UV Index: ~w (~2f)~n", [UVCond, UV]),nl,
    close(Stream),
    write("Weather prediction saved to "), write(File), nl.

% Main user loop 

main_loop :-
    repeat,
    write("Choose an option:"), nl,
    write("1. Enter weather data manually"), nl,
    write("2. Load weather data from file"), nl,
    write("3. Save current prediction to file"), nl,
    write("4. Exit"), nl,
    write("Your choice (1-4): "),
    read(Choice),
    ( Choice == 1 -> get_weather_input
    ; Choice == 2 -> write("Enter filename to load: "), read(FileIn), load_weather_data(FileIn), provide_weather_prediction
    ; Choice == 3 -> write("Enter filename to save prediction: "), read(FileOut), save_weather_prediction(FileOut)
    ; Choice == 4 -> write("Exiting... Goodbye!"), nl, !
    ; write("Invalid choice, try again."), nl, fail 
    ),
    (Choice == 4 -> ! ; fail).

:- initialization(main_loop, main).


