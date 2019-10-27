+++
title = "Apex amazon S3"
description = ""
date = 2016-05-26T09:03:03Z
aliases = []
[extra]
id = 20934
[taxonomies]
categories = []
tags = []
+++

{{omit from|Apex}}

## Apex


```apex

public class amazon{

    public amazon(){}
       
    public void sendTXTToAmazon(Blob b,String docname,String bucket){
        Datetime currentDate = DateTimeHelper.getGMTDate(Datetime.now());      
        
        /* Send to Amazon S3 */
        HttpRequest req = new HttpRequest();         
        AWSAccess__c CS = AWSAccess__c.getOrgDefaults();         
        string amz_content_sha256 = EncodingUtil.convertToHex(Crypto.generateDigest('SHA-256', b));        
        /*Set HTTPRequest Method*/
        req.setMethod('PUT');               
        /*Set HTTPRequest header properties*/
        req.setHeader('content-type', 'text/plain;charset=utf-8');
        req.setHeader('Content-Length',String.valueOf(b.size()));
        req.setHeader('Host','s3-eu-central-1.amazonaws.com');
        req.setHeader('Connection','keep-alive');
        req.setEndpoint('https://s3-eu-central-1.amazonaws.com/'+bucket+'/'+docname);
        req.setHeader('x-amz-content-sha256',amz_content_sha256); 
        req.setHeader('x-amz-storage-class','REDUCED_REDUNDANCY');         
        String todayformat =  currentDate.format('yyyyMMdd');
        req.setHeader('Date',currentDate.format('EEE, d MMM yyyy HH:mm:ss Z')); 
        req.setHeader('x-amz-date',currentDate.format('yyyyMMdd')+'T'+currentDate.format('HHmmss')+'Z'); 
        String key = 'AWS4' +CS.secret__c;
        
        Blob DateKey                 = Crypto.generateMac('hmacSHA256',Blob.valueOf(todayformat),Blob.valueOf(key));
        Blob DateRegionKey           = Crypto.generateMac('hmacSHA256',Blob.valueOf('eu-central-1'),DateKey);
        Blob DateRegionServiceKey    = Crypto.generateMac('hmacSHA256',Blob.valueOf('s3'),DateRegionKey);
        Blob SigningKey              = Crypto.generateMac('hmacSHA256',Blob.valueOf('aws4_request'),DateRegionServiceKey);
        
        String canonical =  'PUT\n'+
                            '/'+bucket+'/'+docname+'\n\n'+                
                            'date:' + currentDate.format('EEE, d MMM yyyy HH:mm:ss Z')+'\n'+
                            'host:s3-eu-central-1.amazonaws.com\n'+
                            'x-amz-content-sha256:'+amz_content_sha256+'\n'+
                            'x-amz-date:'+currentDate.format('yyyyMMdd')+'T'+currentDate.format('HHmmss')+'Z'+'\n'+
                            'x-amz-storage-class:REDUCED_REDUNDANCY\n\n'+                
                            'date;host;x-amz-content-sha256;x-amz-date;x-amz-storage-class\n'
                            +amz_content_sha256;   
        
              
        Blob canonicalsign = Crypto.generateDigest('SHA-256', Blob.valueOf(canonical));  
        Blob StringToSign = Blob.valueOf('AWS4-HMAC-SHA256\n'+
        currentDate.format('yyyyMMdd')+'T'+currentDate.format('HHmmss')+'Z'+'\n'+
        currentDate.format('yyyyMMdd')+'/eu-central-1/s3/aws4_request\n'+
        EncodingUtil.convertToHex(canonicalsign));        
        Blob bsig =  Crypto.generateMac('hmacSHA256',StringToSign,SigningKey);        
        req.setHeader('Authorization', 'AWS4-HMAC-SHA256 Credential='+CS.Key__c+'/'+todayformat+'/eu-central-1/s3/aws4_request,SignedHeaders=date;host;x-amz-content-sha256;x-amz-date;x-amz-storage-class,Signature='+EncodingUtil.convertToHex(bsig));        
        String docBody = EncodingUtil.convertToHex(b);
        
        /*Set the HTTPRequest body */   
        req.setBodyAsBlob(b);   
        Http http = new Http();        
        try {   
            HTTPResponse res = http.send(req);                      
            System.debug(res.getBody());                   
            //System.debug(res.toString());
            System.debug('STATUS:'+res.getStatus());
            
            System.debug('STATUS_CODE:'+res.getStatusCode());                        
        } catch(System.CalloutException e) {           
            System.debug('erreur : '+e);
        }             
        
    }
    public void sendPDFToAmazon(Blob b,String docname,String bucket){
        Datetime currentDate = DateTimeHelper.getGMTDate(Datetime.now());      
        
        /* Send to Amazon S3 */
        HttpRequest req = new HttpRequest();         
        AWSAccess__c CS = AWSAccess__c.getOrgDefaults();         
        string amz_content_sha256 = EncodingUtil.convertToHex(Crypto.generateDigest('SHA-256', b));        
        /*Set HTTPRequest Method*/
        req.setMethod('PUT');               
        /*Set HTTPRequest header properties*/
        req.setHeader('content-type', 'application/pdf');
        req.setHeader('Content-Length',String.valueOf(b.size()));
        req.setHeader('Host','s3-eu-central-1.amazonaws.com');
        req.setHeader('Connection','keep-alive');
        req.setEndpoint('https://s3-eu-central-1.amazonaws.com/'+bucket+'/'+docname);
        req.setHeader('x-amz-content-sha256',amz_content_sha256); 
        req.setHeader('x-amz-storage-class','REDUCED_REDUNDANCY');         
        String todayformat =  currentDate.format('yyyyMMdd');
        req.setHeader('Date',currentDate.format('EEE, d MMM yyyy HH:mm:ss Z')); 
        req.setHeader('x-amz-date',currentDate.format('yyyyMMdd')+'T'+currentDate.format('HHmmss')+'Z'); 
        String key = 'AWS4' +CS.secret__c;
        
        Blob DateKey                 = Crypto.generateMac('hmacSHA256',Blob.valueOf(todayformat),Blob.valueOf(key));
        Blob DateRegionKey           = Crypto.generateMac('hmacSHA256',Blob.valueOf('eu-central-1'),DateKey);
        Blob DateRegionServiceKey    = Crypto.generateMac('hmacSHA256',Blob.valueOf('s3'),DateRegionKey);
        Blob SigningKey              = Crypto.generateMac('hmacSHA256',Blob.valueOf('aws4_request'),DateRegionServiceKey);
        
        String canonical =  'PUT\n'+
                            '/'+bucket+'/'+docname+'\n\n'+                
                            'date:' + currentDate.format('EEE, d MMM yyyy HH:mm:ss Z')+'\n'+
                            'host:s3-eu-central-1.amazonaws.com\n'+
                            'x-amz-content-sha256:'+amz_content_sha256+'\n'+
                            'x-amz-date:'+currentDate.format('yyyyMMdd')+'T'+currentDate.format('HHmmss')+'Z'+'\n'+
                            'x-amz-storage-class:REDUCED_REDUNDANCY\n\n'+                
                            'date;host;x-amz-content-sha256;x-amz-date;x-amz-storage-class\n'
                            +amz_content_sha256;              
        Blob canonicalsign = Crypto.generateDigest('SHA-256', Blob.valueOf(canonical));  
        Blob StringToSign = Blob.valueOf('AWS4-HMAC-SHA256\n'+
        currentDate.format('yyyyMMdd')+'T'+currentDate.format('HHmmss')+'Z'+'\n'+
        currentDate.format('yyyyMMdd')+'/eu-central-1/s3/aws4_request\n'+
        EncodingUtil.convertToHex(canonicalsign));        
        Blob bsig =  Crypto.generateMac('hmacSHA256',StringToSign,SigningKey);        
        req.setHeader('Authorization', 'AWS4-HMAC-SHA256 Credential='+CS.Key__c+'/'+todayformat+'/eu-central-1/s3/aws4_request,SignedHeaders=date;host;x-amz-content-sha256;x-amz-date;x-amz-storage-class,Signature='+EncodingUtil.convertToHex(bsig));        
        String docBody = EncodingUtil.convertToHex(b);
        
        /*Set the HTTPRequest body */   
        req.setBodyAsBlob(b);   
        Http http = new Http();        
        try {   
            HTTPResponse res = http.send(req);                      
            System.debug(res.getBody());                   
            //System.debug(res.toString());
            System.debug('STATUS:'+res.getStatus());
            
            System.debug('STATUS_CODE:'+res.getStatusCode());                    
        } catch(System.CalloutException e) {           
            System.debug('erreur : '+e);
            MailingHelper.sendToAdminMail('Erreur SEND PDF Amazon : '+docname, e.getMessage());            
        }             
        
    }    
    public string GetObjectUrl(String docname,String bucket){
        String url;
        Datetime currentDate = DateTimeHelper.getGMTDate(Datetime.now());
        string exipredFormat = '1600';
        
        String todayformat =  currentDate.format('yyyyMMdd');       
        AWSAccess__c CS = AWSAccess__c.getOrgDefaults();  
        
        String key = 'AWS4' +CS.secret__c;        
        Blob DateKey                 = Crypto.generateMac('hmacSHA256',Blob.valueOf(todayformat),Blob.valueOf(key));
        Blob DateRegionKey           = Crypto.generateMac('hmacSHA256',Blob.valueOf('eu-central-1'),DateKey);
        Blob DateRegionServiceKey    = Crypto.generateMac('hmacSHA256',Blob.valueOf('s3'),DateRegionKey);
        Blob SigningKey              = Crypto.generateMac('hmacSHA256',Blob.valueOf('aws4_request'),DateRegionServiceKey);
        
        String canonical =  'GET\n'+
                            '/'+docname+'\n'+  
                            'X-Amz-Algorithm=AWS4-HMAC-SHA256&'+
                            'X-Amz-Credential='+CS.Key__c+'%2F'+todayformat+'%2Feu-central-1%2Fs3%2Faws4_request&'+
                            'X-Amz-Date='+todayformat+'T'+currentDate.format('HHmmss')+'Z&'+
                            'X-Amz-Expires='+exipredFormat+'&X-Amz-SignedHeaders=host\n'+
                            'host:'+bucket+'.s3.amazonaws.com\n\n'+ 
                            'host\n'+
                            'UNSIGNED-PAYLOAD';  
        system.debug(canonical);
        Blob canonicalsign = Crypto.generateDigest('SHA-256', Blob.valueOf(canonical)); 
       
        Blob StringToSign = Blob.valueOf('AWS4-HMAC-SHA256\n'+
                            todayformat+'T'+currentDate.format('HHmmss')+'Z'+'\n'+
                            todayformat+'/eu-central-1/s3/aws4_request\n'+
                            EncodingUtil.convertToHex(canonicalsign)); 
        
        system.debug(StringToSign);
   
        
        Blob bsig =  Crypto.generateMac('hmacSHA256',StringToSign,SigningKey);         
        
        url = 'https://'+bucket+'.s3.amazonaws.com/'+docname+'?X-Amz-Algorithm=AWS4-HMAC-SHA256&X-Amz-Credential='+CS.Key__c+'/'+todayformat+'/eu-central-1/s3/aws4_request&X-Amz-Date='+todayformat+'T'+currentDate.format('HHmmss')+'Z&X-Amz-Expires='+exipredFormat+'&X-Amz-SignedHeaders=host&X-Amz-Signature='+EncodingUtil.convertToHex(bsig);
              
        return url;
    }
}

```

