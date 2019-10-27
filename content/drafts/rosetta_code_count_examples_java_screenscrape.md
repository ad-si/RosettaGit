+++
title = "Rosetta Code/Count examples/Java/ScreenScrape"
description = ""
date = 2010-04-29T17:10:45Z
aliases = []
[extra]
id = 5396
[taxonomies]
categories = []
tags = []
+++


```java
import java.io.*;
import java.net.URL;
import javax.net.ssl.HttpsURLConnection;

public class ScreenScrape {

	public String read(String sUrl) throws Exception {
		String lastHtml = "";
		BufferedReader reader;
        if (sUrl.startsWith("https://")) { // The URL class doesn't like HTTPS so we have to check for it.
                reader = new BufferedReader(new InputStreamReader(
                                                getSSLOutputCon(sUrl).getInputStream()));
        } else if (sUrl.startsWith("http://")) { // for nonsecure http
                reader = new BufferedReader(new InputStreamReader(
                                                new URL(sUrl).openStream()));
        } else {
        	return "Protocol not supported.  Please verify that your URL starts with "
        			+ "\"http://\" or \"https://\" and try again.";
        }
 
        String line = reader.readLine();
        while (line != null) {
                lastHtml += line;
                line = reader.readLine();
        }
        return lastHtml;
    }
        private HttpsURLConnection httpsUrlCon;
        private HttpsURLConnection getSSLOutputCon(String sUrl) throws Exception {
                try {
                        URL url = new URL(sUrl);
                        httpsUrlCon = (HttpsURLConnection) url.openConnection();
                        httpsUrlCon.setHostnameVerifier(new javax.net.ssl.HostnameVerifier() {
                                public boolean verify(String hostname, javax.net.ssl.SSLSession certHostName) {
                                        return true;
                                }
                        								});
                } catch(Exception e) {
                        throw e;
                }
                return httpsUrlCon;
        }
}
```

