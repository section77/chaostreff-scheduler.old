# [PROTOTYPE] chaostreff scheduler

create chaostreff events in the cms calendar and notify users about events


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

  
## build / install

 * install [stack](https://www.stackage.org/) from [here](https://github.com/commercialhaskell/stack/blob/master/doc/install_and_upgrade.md)
 * stack install

this installs the application under '$HOME/.local/bin'



## config file example

        #
        # chaostreff-scheduler configuration
        # 
        

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
          user: <CMS_USER>
          pass: <CMS_PASS>
        
        
        # 
        reminder-mail:
          enabled: false
          days-ahead: 2
          host: <MAIL_HOST>
          login-user: <MAIL_USER>
          login-pass: <MAIL_PASS>
          sender: chaostreff-event@section77.de
          receiver: section77@j-keck.net
          subject: Uebermorgen ist uebrigens wieder ein Treffen im Hackspace!
        
          # 
          # supported holes in the body:
          #   $receiver$  : the receiver mail address
          #   $title$     : event title
          #   $type$      : event type
          #   $desc$      : event description
          #   $url$       : event url
          #   $cal-title$ : event calendar title
          #   $year$      : event date - year part
          #   $month$     : event date - month part
          #   $day$       : event date - day part
          #   $hour$      : event date - hour part
          #   $minute$    : event date - minute part
          #   $second$    : event date - second part
          #
          body: |
            Titel: $title$
            Datum: $day$.$month$.$year$ um $hour$.$minute$
        
        
            Du weisst schon, ob du kommst?
              ja: https://section77.de/meeting/$year$$month$$day$/$receiver$/ja
              nein: https://section77.de/meeting/$year$$month$$day$/$receiver$/nein
              wer kommt bisher: https://section77.de/meeting/$year$$month$$day$/liste
        
         

## ideas

 * send event notification per twitter?
 * add doodels?
 * send notifcation mail to every user with personalized links
   (-> only one click to show user intention)
     - i'm going: http://xxx/<user-name>/go
     - maybe: http://xxx/<user-name>/maybe 
     - sorry, i'm busy: http://xxx/<user-name>/no
     - who's going: http://xxx/list
   
