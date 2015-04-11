% This is a rebuild of the space api data structure that can be found here: 
% http://spaceapi.net/documentation
% Main data structure is at the end of this file !!!!!

% location
% Position data such as a postal address or geographic coordinates
% Used in:
%         - spacestate
-record(location, {          
          address,                      % String
          latitude,                     % Number
          longitude                     % Number
}).

% spacenet 
% A flag indicating if the hackerspace uses SpaceFED, a federated
% login scheme so that visiting hackers can use the space WiFi 
% with their home space credentials
% Used in:
%         - spacestate
-record(spacenet, {
          spacenet,                     % Boolean
          spacesaml,                    % Boolean
          spacephone                    % Boolean
}).

% stream
% A mapping of stream types to stream URLs.
% Used in:
%         - spacestate
-record(stream, {
          m4,                           % String
          mjpeg,                        % String
          ustream                       % String
}).

% icons 
% Icons that show the status graphically 
% Used in:
%         - state
-record (icons, {
     open,                              % String URL
     closed                             % String URL
}).

% state
% A collection of status-related data: actual open/closed
% status, icons, last change timestamp etc.
% Used in:
%         - spacestate
-record(state, {
          open,                         % [String]
          lastchange,                   % Number
          triggerperson,                % String
          message,                      % String
          icons=#icons{}                % #record icon
}).

% Persons who carry a key and are able to open the space upon 
% request. One of the fields irc_nick, phone, email or twitter
% must be specified.
% Used in:
%         - contact
-record(keymaster, {
     name,                              % String 
     irc_nick,                          % String 
     phone,                             % String 
     email,                             % String 
     twitter                            % String 
}).


% Contact information about your space. You must define at 
% least one which is in the list of allowed values of the 
% issue_report_channels field.
% Used in:
%         - spacestate
-record(contact, {
     phone,                             % String
     sip,                               % String
     keymasters,                        % [#record keymaster]
     irc,                               % String 
     twitter,                           % String 
     facebook,                          % String 
     google,                            % String
     identica,                          % String 
     foursquare,                        % String 
     email,                             % String 
     ml,                                % String 
     jabber,                            % String 
     issue_mail                         % String 
}).

% sensor temperature
% Used in:
%         - sensors 
-record(temperature, {
     value,                             % number
     unit,                              % string
     location,                          % string
     name,                              % string
     description                        % string
}). 

% Sensor type to indicate if a certain door is locked.
% Used in:
%         - sensors
-record(door_locked, {
     value,                             % number
     location,                          % string
     name,                              % string
     description                        % string
}).

% Barometer sensor
% Used in:
%         - sensors
-record(barometer, {
     value,                             % number
     unit,                              % string
     location,                          % string
     name,                              % string
     description                        % string
}).

% radiation details 
% Used in:
%         - radiation
-record(radiation_details, {
     value,                             % number
     unit,                              % string
     dead_time,                         % string
     conversion_factor,                 % string
     location,                          % string
     name,                              % string
     description                        % string
}).

% radiation sensor    
% Used in:
%         - sensors
-record(radiation, {
     alpha=#radiation_details{},        % radiation details
     beta=#radiation_details{},         % radiation details
     gamma=#radiation_details{},        % radiation details
     beta_gamma=#radiation_details{}    % radiation details

}).

% sensor humidity 
% Used in:
%         - sensors
-record(humidity, {
     value,                             % number
     unit,                              % string
     location,                          % string
     name,                              % string
     description                        % string
}).



% Data of various sensors in your space (e.g. temperature, humidity,
% amount of Club-Mate left, …). The only canonical property is the
% temp property, additional sensor types may be defined by you. In
% this case, you are requested to share your definition for inclusion
% in this specification.
% Used in:
%         - spacestate
-record(sensors, {
     temperature=[],                    % array of record temperature sensors
     door_locked=[],                    % array of record door is locked sensors
     barometers=[],                     % array of record barometer sensors 
     radiation=[],                      % array of record radiation sensors 
     humidity,
     beverage_supplys,
     power_consumptions,
     winds,
     network_connectionss,
     account_balances,
     total_member_count,
     people_now_present
}).


% body
-record(spacestate, {
     api,                               % String 
     name,                              % String 
     logo,                              % String URL 
     url,                               % String URL
     location=#location{},              % #record location
     spacenet=#spacenet{},              % #record spacenet
     cams=[],                           % [String URL]
     stream=#stream{},                  % #record stream
     state=#state{},                    % #record state
     events=[],                         % [#record event]
     contact=#contact{},                % #record contact
     issue_report_channels=[],          % [String URL]
     sensors=#sensors{},                % #record sensor
     feeds,                             % #record feeds
     cache,                             % #record cache
     projects,                          % [String URL]
     radioshows                         % [#record radioshow]
}).

% events 
% Events which happened recently in your space and which could 
% be interesting to the public, like 'User X has entered/triggered/did 
% something at timestamp Z'
-record(event, {
     name,                    % String  
     type,                    % String  
     timeStamp,               % Number
     extra                    % String 
}). 
  
% sensor beverage_supply
-record(beverage_supply, {
     value,                   % number
     unit,                    % string
     location,                % string
     name,                    % string
     description              % string
}).

% sensor power_consumption
-record (power_consumption, {
     value,                   % number
     unit,                    % string
     location,                % string
     name,                    % string
     description              % string
}).

% sensor wind
-record (wind, {
     properties,              % string
     location,                % string
     name,                    % string
     description              % string
}).

% wind properties
-record (properties, {
     speed,                   % value & unit
     gust,                    % value & unit
     direction,               % value & unit
     elevation                % value & unit
}).

% value and unit for  speed, gust, direction and elevation
-record (value_unit, {
     value,                   % number
     unit                     % string
}).

% seonsor network_connection
-record (network_connection, {
     type,                    % string
     value,                   % number
     machines,                % string            
     location,                % string
     name,                    % string
     description              % string
     }).

% network machines
-record (machines, {
     name,                     % string
     mac                       % string
}).   

% sensor account_balance
-record(account_balance, {
     value,                   % number
     unit,                    % string
     location,                % string
     name,                    % string
     description              % string
}).

% sensor total_member_count
-record(total_member_count, {
     value,                   % number
     location,                % string
     unit,                    % string
     name,                    % string
     description              % string
}).

% sensor people_now_present
-record(people_now_present, {
     value,                   % number
     location,                % string
     unit,                    % string
     name,                    % string
     names,                   % [String]
     description              % string
}).

% feeds,
-record (feeds, {
     blog,                    % blog feed
     wiki,                    % blog wiki
     calendar,                % blog calendar
     flickr                   % blog flickr
}).

% blog
-record (blog, {
     type,                    % string
     url                      % string
}).

% wiki
-record (wiki, {
     type,                    % string
     url                      % string
}).

% calendar
-record (calendar, {
     type,                    % string
     url                      % string
}).

% flickr
-record (flickr, {
     type,                    % string
     url                      % string
}).

% cache, allowed values for schedule : 
% m.02, m.05, m.10, m.15, m.30, h.01, h.02, h.04, h.08, h.12, d.01
-record (cache, {
     schedule                 % string
}).

% radioshow
-record(radioshow, {
          name,               %string
          url,                %string
          type,               %string
          starter,            %string
          ender               %string
}).