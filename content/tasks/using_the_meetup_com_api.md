+++
title = "Using the Meetup.com API"
description = ""
date = 2014-12-30T16:29:06Z
aliases = []
[extra]
id = 18440
[taxonomies]
categories = ["task"]
tags = []
+++

To:

<ol>
<li>Create a 'get' function for the API, that can be used to get a list of events for example.</li>
<li>Create a 'post' function for the API, that can be used to create a new venue or event for example.</li>
</ol>

Using the [http://www.meetup.com/meetup_api/ Meetup.com API].

Both functions take two parameters, 'details' and 'url'. Details is an object containing required and optional arguments specified by the API's requirement (Example, [http://www.meetup.com/meetup_api/docs/2/open_events/ open events] requires at least one detail). The 'url' supplied informs the get/post method what part of the API to ping (Example, valid URLs for 'get' could be ['/2/open_events' http://www.meetup.com/meetup_api/docs/2/open_events/] or ['/comments' http://www.meetup.com/meetup_api/docs/comments/]).

Some posting features are paid, such as creating an event. [Editing member details http://www.meetup.com/meetup_api/docs/2/member/#edit] for example, is an example of a free 'post' API.

An API key is assumed to be supplied through an <code>api_key.txt</code> file.

Solutions should be implemented without any meetup.com helper libraries, to make it easier to translate to languages which don't have such helper libraries. However, examples that do use helper libraries may be provided as an addition.

This task was created through [https://www.google-melange.com/gci/task/view/google/gci2014/5820245222621184 Google Code-in].



## Go

```go
package main

import (
	"bytes"
	"encoding/json"
	"fmt"
	"io/ioutil"
	"log"
	"net/http"
	"net/url"
	"os"
	"strings"
	"time"
)

var key string

func init() {
	// Read an API key from the specified file.
	// See www.meetup.com/meetup_api/auth for other ways to authenticate.
	const keyFile = "api_key.txt"
	f, err := os.Open(keyFile)
	if err != nil {
		log.Fatal(err)
	}
	keydata, err := ioutil.ReadAll(f)
	if err != nil {
		log.Fatal(err)
	}
	key = strings.TrimSpace(string(keydata))
}

type EventResponse struct {
	Results []Result
	// … other fields …
}

type Result struct {
	ID          string
	Status      string
	Name        string
	EventURL    string `json:"event_url"`
	Description string
	Time        EventTime
	// … other fields …
}

// EventTime is a time.Time that will be marshalled/unmarshalled to/from JSON
// as a UTC time in milliseconds since the epoch as returned by the Meetup API.
type EventTime struct{ time.Time }

func (et *EventTime) UnmarshalJSON(data []byte) error {
	var msec int64
	if err := json.Unmarshal(data, &msec); err != nil {
		return err
	}
	et.Time = time.Unix(0, msec*int64(time.Millisecond))
	return nil
}

func (et EventTime) MarshalJSON() ([]byte, error) {
	msec := et.UnixNano() / int64(time.Millisecond)
	return json.Marshal(msec)
}

// String formats a Result suitable for debugging output.
func (r *Result) String() string {
	var b bytes.Buffer
	fmt.Fprintln(&b, "ID:", r.ID)
	fmt.Fprintln(&b, "URL:", r.EventURL)
	fmt.Fprintln(&b, "Time:", r.Time.Format(time.UnixDate))
	d := r.Description
	const limit = 65
	if len(d) > limit {
		d = d[:limit-1] + "…"
	}
	fmt.Fprintln(&b, "Description:", d)
	return b.String()
}

func main() {
	v := url.Values{
		//"topic": []string{"tech"},
		//"city":  []string{"Barcelona"},
		"topic": []string{"photo"},
		"time":  []string{",1w"},
		"key":   []string{key},
	}
	u := url.URL{
		Scheme:   "http",
		Host:     "api.meetup.com",
		Path:     "2/open_events.json",
		RawQuery: v.Encode(),
	}
	//log.Println("API URL:", u.String())

	resp, err := http.Get(u.String())
	if err != nil {
		log.Fatal(err)
	}
	defer resp.Body.Close()
	log.Println("HTTP Status:", resp.Status)

	body, err := ioutil.ReadAll(resp.Body)
	if err != nil {
		log.Fatal(err)
	}

	//log.Printf("Body: %q\n", body)
	var buf bytes.Buffer
	if err = json.Indent(&buf, body, "", "  "); err != nil {
		log.Fatal(err)
	}
	//log.Println("Indented:", buf.String())

	var evresp EventResponse
	json.Unmarshal(body, &evresp)
	//log.Printf("%#v\n", evresp)

	fmt.Println("Got", len(evresp.Results), "events")
	if len(evresp.Results) > 0 {
		fmt.Println("First event:\n", &evresp.Results[0])
	}
}
```

```txt

2014/12/28 19:16:01 HTTP Status: 200 OK
Got 200 events
First event:
 ID: 219254566
URL: http://www.meetup.com/Bay-Area-Photography-Shoots-and-Workshops/events/219254566/
Time: Sun Dec 28 16:00:00 EST 2014
Description: <p><b>Beginners Lighting:</b> join us for this exciting workshop…

```



## Java


EventGetter.java


```java
package src;

import java.io.BufferedReader;
import java.io.FileReader;
import java.net.URI;

import org.apache.http.client.methods.CloseableHttpResponse;
import org.apache.http.client.methods.HttpGet;
import org.apache.http.client.utils.URIBuilder;
import org.apache.http.impl.client.CloseableHttpClient;
import org.apache.http.impl.client.HttpClients;
import org.apache.http.util.EntityUtils;



public class EventGetter {
	

	String city = "";
	String topic = "";
	
	public String getEvent(String path_code,String key) throws Exception{
		String responseString = "";
		
		URI request = new URIBuilder()			//We build the request URI
			.setScheme("http")
			.setHost("api.meetup.com")
			.setPath(path_code)
			//List of parameters :
			.setParameter("topic", topic)
			.setParameter("city", city)
			//End of params
			.setParameter("key", key)
			.build();
		
		HttpGet get = new HttpGet(request);			//Assign the URI to the get request
		System.out.println("Get request : "+get.toString());
		
		CloseableHttpClient client = HttpClients.createDefault();
		CloseableHttpResponse response = client.execute(get);
		responseString = EntityUtils.toString(response.getEntity());
		
		return responseString;
	}
	
	public String getApiKey(String key_path){
		String key = "";
		
		try{
			BufferedReader reader = new BufferedReader(new FileReader(key_path));	//Read the file where the API Key is
			key = reader.readLine().toString();									//Store key
			reader.close();
		}
		catch(Exception e){System.out.println(e.toString());}
		
		return key;																//Return the key value.
	}
	
}
```


Main.java

```java
/*
 * In this class, You can see the diferent 
 * ways of asking for events.
 * */


package src;

import java.util.Iterator;

import org.json.simple.JSONArray;
import org.json.simple.JSONObject;
import org.json.simple.JSONValue;
import org.json.simple.parser.JSONParser;

public class Main {
	public static void main(String[] args) {
		
		String key_path = "API_key/api_key.txt"; 							//Path to API Key (api_key.txt)
		String key = "";
		String path_code = "/2/open_events";								//PathCode for get-events
																			//More PathCodes : http://www.meetup.com/meetup_api/docs/
		String events = "";
		
		EventGetter eventGetter = new EventGetter();
		key = eventGetter.getApiKey(key_path);
		
		/*
		 * 1-PARAMETER EXAMPLE :
		 */ 
		eventGetter.topic = "photo";										//Set the parameter "topic" to "photo"
		
		try {
			events = eventGetter.getEvent(path_code, key);					//Store the event response into a String
		} catch (Exception e) {e.printStackTrace();}
		DecodeJSON(events);													//Print JSON-parsed events info						
		
		/*
		 * 2-PARAMETER EXAMPLE :
		 */
		eventGetter.topic = "tech";											//Set parameters
		eventGetter.city = "Barcelona";										
		try{
			events = eventGetter.getEvent(path_code, key);
		}catch(Exception e){e.printStackTrace();}
		//System.out.println(events);											//Print the events list (JSON)
	
		
		/*
		 * MULTIPLE-TOPICS EXAMPLE :
		 * Separate topics by commas
		 */
		eventGetter.topic = "tech,photo,art";								//multiple topic separated by commas										
		eventGetter.city = "Barcelona";
		try{
			events = eventGetter.getEvent(path_code, key);
		}catch(Exception e){e.printStackTrace();}
		
	}
	
	public static void DecodeJSON(String events){
		
		try{
			JSONParser parser = new JSONParser();
			JSONObject obj = (JSONObject) parser.parse(events);
			JSONArray results = (JSONArray) obj.get("results");
			System.out.println("Results : ");
			
			Iterator i = results.iterator(); 
			while(i.hasNext()){
				JSONObject event = (JSONObject) i.next();
				System.out.println("Name : "+event.get("name"));
				
				if(event.containsKey("venue")){
					JSONObject venue = (JSONObject) event.get("venue");
					System.out.println("Location (city) : "+venue.get("city"));
					System.out.println("Location (adress) : "+venue.get("adress_1"));
				}
				
				
				System.out.println("Url : "+event.get("event_url"));
				System.out.println("Time : "+event.get("time"));
				i.next();
			}
			
		}
		catch(Exception e){e.printStackTrace();}
	}

}
```



## JavaScript


Made on node js. Run using 'node filename.js'


```javascript
var fs = require('fs');
var request = require('request');

var meetup = function() {
  var key = fs.readFileSync('api_key.txt', 'utf-8');
  var url = "https://api.meetup.com";

  var composeURL = function(root, object) {
    return root + '?' + JSON.stringify(object).replace(/":"/g, '=').replace(/","/g, '&').slice(2, -2)
  }

  var get = function(params, callback, path) {
    params.key = key;

    request.get(composeURL(url + (path || '/2/open_events'), params), function(err, res, body) {
      if ( err ) {
        console.error(err);
        return false;
      }


      callback(JSON.parse(body)['results']);
    })
  }


  var post = function(details, callback, path) {
    details.key = key;

    request.post({
      headers: { 'content-type' : 'application/x-www-form-urlencoded' },
      url: url + (path || '/2/event'),
      form: details
    }, function(err, res, body) {
      callback(body);
    })
  }

  var parseEvent = function(mEvent) {
    /*
     * A simple function that converts JSON to 
     * string in a pretty way
    **/
    var name = mEvent['name'] || '';
    var desc = mEvent['desc'] || '';
    var url = mEvent['url'] || '';

    if ( mEvent['venue'] ) {
      var city = mEvent['venue']['city'] || '';
      var lat = mEvent['venue']['lat'] || '';
      var lon = mEvent['venue']['lon'] || '';
    }
    
    if ( mEvent['group'] )
      var group = mEvent['group']['name'] || '';

    var parsed = '';

    if ( name ) parsed += 'Name: ' + name + '\n';
    if ( desc ) parsed += 'Description: ' + desc + '\n';
    if ( url ) parsed += 'Url: ' + url + '\n';
    if ( city ) parsed += 'City: ' + city + '\n';
    if ( lat ) parsed += 'Latitude: ' + lat + '\n';
    if ( lon ) parsed += 'Longitude: ' + lon + '\n';
    if ( group ) parsed += 'Group: ' + group + '\n';

    return parsed;

  };

  var parseEvents = function(results) {
    console.log('a');
    for ( var i = 0; i < results.length; i++ ) {
      console.log( parseEvent(results[i]) );
    }
  }

  return {
    get: get,
    parseEvents: parseEvents,
    post: post
  }
}



meetup().get({
  // More Info: http://www.meetup.com/meetup_api/docs/2/open_events/
  topic: 'photo',
  city: 'nyc'
}, function(results) {
  meetup().parseEvents(results);
});


/*
 * Getting group ID and group urlname
 *
 * The URL name is simply the part after meetup.com/ on a meetup group.
 * Example, ID of meetup.com/foodie-programmers is 'foodie-programmers'.
 *
 * Running the code below with the group name will give the group ID, an integer.

meetup().get({
  'group_urlname': 'foodie-programmers'
}, function(group) {
  console.log(group.id);
}, '/2/groups');

 * Using the above group_id and the group_urlname manually, 
 * you can post events to a group with the below code
**/

meetup().post({
  // More Info: http://www.meetup.com/meetup_api/docs/:urlname/venues/#create
  name: 'Finding Nemo',
  address_1: 'p sherman 42 wallaby way sydney',
  city: 'sydney',
  country: 'australia',
  // state: needed if in US or CA.
}, function(venue) {
  console.log('Venue: ', venue, venue.id); 
  // Prints a venue ID that can be used to create a event
}, '/' + '{{ foodie-programmers }}' + '/venues'); 
// This needs a valid urlname for the group


meetup().post({
  // More Info: http://www.meetup.com/meetup_api/docs/2/groups/
  group_id: 42, // Group ID goes here
  group_urlname: 'foodie-programmers',
  name: 'Tomato Python Fest',
  description: 'Code vegetables in Python! Special speech by Guido Van Ossum',
  duration: 1000 * 60 * 60 * 2, // Duration in milliseconds
  time: 1419879086343, // Milliseconds since epoch
  why: 'We should do this because... Less than 250 characters',
  hosts: 'up to 5 comma separated member ids',
  venue_id: 42, // Integer, ID of venue. Venue can be created with the above.
  lat: 42, // Latitude, Integer
  lon: 42, // Longitude, Integer
  simple_html_description: 'Event description in <b>simple html</b>. Less than <i>50000</i> characters.'
}, function(result) {
  console.log('Event: ', result);
})
```



## Python


eventGetter.py

```python
#http://docs.python-requests.org/en/latest/
import requests
import json

city = None
topic = None

def getEvent(url_path, key) :
    responseString = ""
    
    params = {'city':city, 'key':key,'topic':topic}
    r = requests.get(url_path, params = params)    
    print(r.url)    
    responseString = r.text
    return responseString


def getApiKey(key_path):
    key = ""
    f = open(key_path, 'r')
    key = f.read()
    return key


def submitEvent(url_path,params):
    r = requests.post(url_path, data=json.dumps(params))        
    print(r.text+" : Event Submitted")
```


main.py

```python
import eventGetter as eg
import json

def main():
    url_path = "https://api.meetup.com"         #Url to meetup API
    key_path = "api_key.txt"                    #Path to api_key.txt
    path_code = ""                              #var to store the url_path + the specific api path
    key = eg.getApiKey(key_path)    
    
    #1-parameter get events example : 
    print("1-PARAMETER EXAMPLE")
    path_code = url_path+"/2/open_events"
    eg.topic = "photo"
    response = eg.getEvent(path_code, key)
    decodeJSON(response)

    #2-parameter get events example :
    print("\n")
    print("2-PARAMETER EXAMPLE") 
    path_code = url_path+"/2/open_events"
    eg.topic = "photo"
    eg.city = "nyc"
    response = eg.getEvent(path_code, key)
    decodeJSON(response) 

    #Get GEO Example : 
    print("\n")
    print("Get GEO Example")
    path_code = url_path+"/2/open_events"
    eg.topic = "photo"
    eg.city = None
    exclude = None
    response = eg.getEvent(path_code, key)
    decodeGEO(response)


    #Exclude topics Example
    print("\n")
    print("EXCLUDE-TOPICS EXAMPLE")
    path_code = url_path+"/2/open_events"
    eg.topic = "photo"
    eg.city = None
    exclude = "club"
    response = eg.getEvent(path_code, key)
    decodeJSONExcluding(response, exclude)
    
    
    
def decodeJSON(response):
    j = json.loads(response.encode('ascii','ignore').decode())   #This is a Python Dict (JSON array)
    i = 0
    results = j['results']
    while i<len(results):
        event = results[i]
        print("Event "+str(i))
        print("Event name : "+event['name'])
        print("Event URL : "+event['event_url'])
        try : 
            print("City : "+str(event['venue']['city']))
        except KeyError : 
            print("This event has no location assigned")
            
        try :
            print("Group : "+str(event['group']['name']))
        except KeyError :
            print("This event is not related to any group")            
            
        i+=1
        

def decodeJSONExcluding(response, exclude):
    j = json.loads(response.encode('ascii','ignore').decode())   #This is a Python Dict (JSON array)
    i = 0
    results = j['results']
    while i<len(results):
        event = results[i]
        if 'description' in event : 
            if exclude not in str(event['description']) : 
                print("Event "+str(i))            
                print("Event name : "+event['name'])
                print("Event URL : "+event['event_url'])
                try : 
                    print("City : "+str(event['venue']['city']))
                except KeyError : 
                    print("This event has no location assigned")
            
                try :
                    print("Group : "+str(event['group']['name']))
                except KeyError :
                    print("This event is not related to any group")            
        
            else :
                print("Event number "+str(i)+" is excluded by its keywords")
        
        i+=1
        
        
def decodeGEO(response):
    j = json.loads(response.encode('ascii','ignore').decode())   #This is a Python Dict (JSON array)
    i = 0
    results = j['results']
    while i<len(results):
        event = results[i]
        print("Event "+str(i))            
        print("Event name : "+event['name'])
        try : 
            print("Lat : "+str(event['venue']['lat']))
            print("Lon : "+str(event['venue']['lon']))
        except KeyError : 
            print("This event has no location assigned")
        
        i+=1
        

    
main()
```

