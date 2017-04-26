# chaostreff scheduler

schedule chaostreff events in the cms calendar


## usage

  * create events for the current year / month

        chaostreff-scheduler <CONFIG_FILE>

  * create events for the current and next (COUNT -1) months

        chaostreff-scheduler <CONFIG_FILE> <COUNT>

  * create events for the given year / month

        chaostreff-scheduler <CONFIG_FILE> <YEAR> <MONTH>


## example

* schedule a given year / month

        ./chaostreff-scheduler  chaostreff-scheduler.yaml-local 2016 01
        event scheduled at: 2016-02-13 20:00:00, cms msg: "Event: Chaostreff has been added"
        event scheduled at: 2016-02-27 20:00:00, cms msg: "Event: Chaostreff has been added"


* schedule the current and next month
  (result from a run at january 2016 where three events was already scheduled)

        ./chaostreff-scheduler <CONFIG_FILE> 2
        already scheduled: 2016-01-13
        already scheduled: 2016-01-27
        already scheduled: 2016-02-10
        event scheduled at: 2016-02-24 20:00:00, cms msg: "Event: Chaostreff has been added"


## build

 * install [stack](https://www.stackage.org/) from [here](https://github.com/commercialhaskell/stack/blob/master/doc/install_and_upgrade.md)
 * stack build



## config file example

        # event template
        event-template:
          title: Chaostreff
          time: 20:00:00
          type: Chaostreff im Hackspace
          url: http://osm.org/go/0DLdIKvND?way=286246667
          desc: Dienstagstreffen des Vereins im Hackspace (wie jeden 1. und 3. (manchmal auch 5.) Dienstag des Monats)
          calendar-title: Chaostreff

        # login for cms (to update the calendar)
        cms-login:
          user: <USER>
          pass: <PASS>


## ideas

 * send event notification per twitter?
 * add doodels?
 * send notifcation mail to every user with personalized links (**wip in the dev branch**)
   (-> only one click to show user intention)
     - i'm going: http://xxx/<user-name>/go
     - maybe: http://xxx/<user-name>/maybe
     - sorry, i'm busy: http://xxx/<user-name>/no
     - who's going: http://xxx/list
