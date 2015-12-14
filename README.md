# [PROTOTYPE] chaostreff scheduler

create chaostreff events in the cms calendar


## usage

  * create events for the current year / month

        chaostreff-scheduler <CONFIG_FILE> 

  * create events for the given year / month

        chaostreff-scheduler <CONFIG_FILE> <YEAR> <MONTH>


## example

for a month, where one event are already scheduled

        ./chaostreff-scheduler <CONFIG_FILE> 2015 12
        already scheduled: 2015-12-09
        event scheduled at: 2015-12-23 20:00:00, cms msg: "Event: Chaostreff has been added"

  
## build

 * install [stack](https://www.stackage.org/) from [here](https://github.com/commercialhaskell/stack/blob/master/doc/install_and_upgrade.md)
 * stack build



## config file example

        # event template
        event-template:
          title: Chaostreff
          time: 20:00:00
          type: Chaostreff im Hackspace
          desc: Mittwochstreffen des Vereins im Hackspace (wie jeden 2. und 4. Mittwoch des Monats)
          url: http://osm.org/go/0DLdM4FF2--?way=297085686"
          calendar-title: Chaostreff
        
  
        # login for cms (to update the calendar)
        cms-login:
          user: <USER>
          pass: <PASS>


## ideas

 * send notification mail two days before the event
 * send event notification per twitter?
 * add doodels?
 * send notifcation mail to every user with personalized links
   (-> only one click to show user intention)
     - i'm going: http://xxx/<user-name>/go
     - maybe: http://xxx/<user-name>/maybe 
     - sorry, i'm busy: http://xxx/<user-name>/no
     - who's going: http://xxx/list
   
