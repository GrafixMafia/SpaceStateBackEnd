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
          events,             % [#record event]
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
% Events which happened recently in your space and which could 
% be interesting to the public, like 'User X has entered/triggered/did 
% something at timestamp Z'
-record(event, {
     name,                    % String  
     type,                    % String  
     timeStamp,               % Number
     extra                    % String 
}).


-record(contact, {
     phone,                   % String
     sip,                     % String
     keymasters,              % [#record keymaster]
     irc,                     % String 
     twitter,                 % String 
     facebook,                % String 
     google,                  % [String]
     identica,                % String 
     foursquare,              % String 
     email,                   % String 
     ml,                      % String 
     jabber,                  % String 
     issue_mail              % String 
}).

-record(keymaster, {
     name,                    % String 
     irc_nick,                % String 
     phone,                   % String 
     email,                   % String 
     twitter                  % String 
}).

-record(issue_report_channels, {
     issue_mail
}).

% sensors,

-record(sensors, {
     temperature,
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
     alpha,                   %radiation details
     beta,                    %radiation details
     gamma,                   %radiation details
     beta_gamma               %radiation details

}).

% radiation details 
-record(radiation_details, {
     value,                   % number
     unit,                    % string
     dead_time,               % string
     conversion_factor        % string
     location,                % string
     name,                    % string
     description              % string
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