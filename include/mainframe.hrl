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

% Events which happened recently in your space and which could 
% be interesting to the public, like 'User X has entered/triggered/did 
% something at timestamp Z'
-record(event, {
     name,                              % String  
     type,                              % String  
     timeStamp,                         % Number
     extra                              % String 
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
     unit,                              % String
     location,                          % String
     name,                              % String
     description                        % String
}). 

% Sensor type to indicate if a certain door is locked.
% Used in:
%         - sensors
-record(door_locked, {
     value,                             % number
     location,                          % String
     name,                              % String
     description                        % String
}).

% Barometer sensor
% Used in:
%         - sensors
-record(barometer, {
     value,                             % number
     unit,                              % String
     location,                          % String
     name,                              % String
     description                        % String
}).

% radiation details 
% Used in:
%         - radiation
-record(radiation_details, {
     value,                             % number
     unit,                              % String
     dead_time,                         % String
     conversion_factor,                 % String
     location,                          % String
     name,                              % String
     description                        % String
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
     unit,                              % String
     location,                          % String
     name,                              % String
     description                        % String
}).

% sensor beverage_supply
% Used in:
%         - sensors
-record(beverage_supply, {
     value,                             % number
     unit,                              % String
     location,                          % String
     name,                              % String
     description                        % String
}).

% sensor power_consumption
% Used in:
%         - sensors
-record (power_consumption, {
     value,                             % number
     unit,                              % String
     location,                          % String
     name,                              % String
     description                        % String
}).


% value and unit for  speed, gust, direction and elevation
% Used in:
%         - properties
-record (value_unit, {
     value,                             % number
     unit                               % String
}).

% wind properties
% Used in:
%         - wind
-record (properties, {
     speed,                            % value & unit
     gust,                             % value & unit
     direction,                        % value & unit
     elevation                         % value & unit
}).

% sensor wind
% Used in:
%         - sensors
-record (wind, {
     properties=#properties{},          % String
     location,                          % String
     name,                              % String
     description                        % String
}).

% network machines
% Used in:
%         - network_connection
-record (machines, {
     name,                              % String
     mac                                % String
}). 

% seonsor network_connection
% Used in:
%         - sensors
-record (network_connection, {
     type,                              % String
     value,                             % number
     machines=[],                       % list of record machines sensors           
     location,                          % String
     name,                              % String
     description                        % String
}).

% sensor account_balance
% Used in:
%         - sensors
-record(account_balance, {
     value,                             % number
     unit,                              % String
     location,                          % String
     name,                              % String
     description                        % String
}).

% sensor total_member_count
% Used in:
%         - sensors 
-record(total_member_count, {
     value,                             % number
     location,                          % String
     unit,                              % String
     name,                              % String
     description                        % String
}).

% sensor people_now_present
-record(people_now_present, {
     value,                             % number
     location,                          % String
     unit,                              % String
     name,                              % String
     names,                             % [String]
     description                        % String
}).
% Data of various sensors in your space (e.g. temperature, humidity,
% amount of Club-Mate left, …). The only canonical property is the
% temp property, additional sensor types may be defined by you. In
% this case, you are requested to share your definition for inclusion
% in this specification.
% Used in:
%         - spacestate
-record(sensors, {
     temperature=[],                    % list of record temperature sensors
     door_locked=[],                    % list of record door is locked sensors
     barometers=[],                     % list of record barometer sensors 
     radiation=[],                      % list of record radiation sensors 
     humidity=[],                       % list of record humidity sensors 
     beverage_supplys=[],               % list of record beverage_supplys sensors 
     power_consumptions=[],             % list of record power_consumptions sensors
     winds=[],                          % list of record winds sensors
     network_connectionss=[],           % list of record network_connectionss sensors
     account_balances=[],               % list of record account_balances sensors
     total_member_count=[],             % list of record total_member_count sensors
     people_now_present=[]              % list of record people_now_present sensors
}).

% feed_details
% Used in:
%         - feeds
-record (feed_details, {
     type,                              % String
     url                                % String
}).


% Feeds where users can get updates of your space
% Used in:
%         - spacestate
-record (feeds, {
     blog=#feed_details{},              % blog feed
     wiki=#feed_details{},              % blog wiki
     calendar=#feed_details{},          % blog calendar
     flickr=#feed_details{}             % blog flickr
}).

% cache, allowed values for schedule : 
% m.02, m.05, m.10, m.15, m.30, h.01, h.02, h.04, h.08, h.12, d.01
% Used in:
%         - spacestate
-record (cache, {
     schedule                           % String
}).

% radioshow
-record(radioshow, {
          name,                         % String
          url,                          % String
          type,                         % String
          starter,                      % String
          ender                         % String
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
     feeds=#feeds{},                    % #record feeds
     cache=#cache{},                    % #record cache
     projects=[],                       % [String URL]
     radioshows=[]                      % [#record radioshow]
}).