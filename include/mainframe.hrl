% This is a rebuild of the space api data structure that can be found here: 
% http://spaceapi.net/documentation

% body
-record(space, {
          api,                % String 
          name,               % String 
          logo,               % String URL 
          url,                % String URL
          location,           % #record location
          spacefed,           % #record spacenet
          cams,               % [String URL]
          stream,             % #record stream
          state,              % #record state
          events,             % [#record events]
          contact,            % #record contact
          issue_report_channels, % [String]
          sensors,            % #record sensor
          feeds,              % #record feeds
          cache,              % #record cache
          projects,           % [String URL]
          radioshows          % [#record radioshow]
}).

% location
% Position data such as a postal address or geographic coordinates
-record(location, {          
          address,            % String
          latitude,           % Number
          longitude           % Number
}).

% spacenet 
% A flag indicating if the hackerspace uses SpaceFED, a federated
% login scheme so that visiting hackers can use the space WiFi 
% with their home space credentials
-record(spacenet, {
          spacenet,           % Boolean
          spacesaml,          % Boolean
          spacephone          % Boolean
}).

% stream
% A mapping of stream types to stream URLs.
-record(stream, {
          m4,                 % String
          mjpeg,              % String
          ustream             % String
}).

% state
% A collection of status-related data: actual open/closed
% status, icons, last change timestamp etc.
-record(state, {
          open,               % [String]
          lastchange,         % Number
          triggerperson,      % String
          message,            % String
          icons               % #record icon
}).

% icon 
% Icons that show the status graphically
-record (icon, {
     open,                    % String URL
     closed                   % String URL
}).

% events
-record(event, {
     name, 
     type, 
     timeStamp, 
     extra 
}).

-record(contact, {
     phone,
     sip,
     keymasters,
     irc,
     twitter,
     facebook,
     google,
     identica,
     foursquare,
     email,
     ml,
     jabber,
     issue_mail,
}).

-record(issue_report_channels, {
     issue_mail
}).

% sensors,

-record(sensors, {
     temperature,, 
     door_locked,
     barometers, 
     radiation,
     humiditys,
     beverage_supplys,
     power_consumptions,
     winds,
     network_connectionss,
     account_balances,
     total_member_count,
     people_now_present
}).

-record(temperature, {
     value,                   % number
     unit,                    % string
     location,                % string
     name,                    % string
     description              % string
}). 

-record(door_locked, {
     value,                   % number
     location,                % string
     name,                    % string
     description              % string
}).

% sensor barometer
-record(barometer, {
     value,                   % number
     unit,                    % string
     location,                % string
     name,                    % string
     description              % string
}). 
  
% sensor    
-record(radiation, {
     alpha

}).

% sensor 
-record(radiation, {

}).

% sensor humidity 
-record(humidity, {
     value,                   % number
     unit,                    % string
     location,                % string
     name,                    % string
     description              % string
}).

% sensor beverage_supply
-record(beverage_supply, {

}).
     % sensor 
     power_consumption,
     % sensor 
     wind,
     % sensor 
     network_connections,

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
% cache,
% projects,

% radioshow
-record(radioshow, {
          name,
          url,
          type,
          starter,
          ender
}).








