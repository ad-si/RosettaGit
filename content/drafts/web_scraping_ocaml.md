+++
title = "Web scraping/OCaml"
description = ""
date = 2010-02-06T15:14:51Z
aliases = []
[extra]
id = 3044
[taxonomies]
categories = []
tags = []
+++

The content of this page is related to the main page [[Web Scraping#OCaml]]


```ocaml

let init_socket addr port =
  let inet_addr = (Unix.gethostbyname addr).Unix.h_addr_list.(0) in
  let sockaddr = Unix.ADDR_INET (inet_addr, port) in
  let suck = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Unix.connect suck sockaddr;
  let outchan = Unix.out_channel_of_descr suck in
  let inchan = Unix.in_channel_of_descr suck in
  (inchan, outchan)
;;

let serialize ~post_data =
  String.concat "&"
    (List.map (fun (key, var) -> key ^ "=" ^ var) post_data)
;;

type request = GET | HEAD | POST of (string * string) list

let submit_request ~address ~port ~kind ~path ~referer ~user_agent =
  let req_tag, post_data =
    match kind with
    | GET -> "GET", None
    | HEAD -> "HEAD", None
    | POST data -> "POST", Some data
  in
  let request =
    (Printf.sprintf "%s %s HTTP/1.0\r\n" req_tag path) ^
    (Printf.sprintf "Host: %s\r\n" address) ^
    (match user_agent with None -> "" | Some ua -> Printf.sprintf "User-Agent: %s\r\n" ua) ^
    (match referer with None -> "" | Some referer -> Printf.sprintf "Referer: %s\r\n" referer) ^
    (match post_data with None -> ""
     | Some post_data -> let post_data = serialize ~post_data in
         "Content-type: application/x-www-form-urlencoded\r\n" ^
         "Content-length: "^ string_of_int(String.length post_data) ^"\r\n" ^
         "Connection: close\r\n" ^
         "\r\n" ^
         post_data
    ) ^
    ("\r\n")
  in
  let (inchan, outchan) = init_socket address port in
  output_string outchan request;
  flush outchan;
  (inchan, outchan)
;;

let strip_cr str =
  let len = String.length str in
  let striped = String.create len in
  let rec aux i j =
    if i >= len then j else begin
      if str.[i] <> '\r' then begin
        striped.[j] <- str.[i];
        aux (succ i) (succ j)
      end else begin
        aux (succ i) j
      end
    end
  in
  let nlen = aux 0 0 in
  (String.sub striped 0 nlen)
;;

let cont_of_inchan ?limit ic =
  let first_line = strip_cr(input_line ic) in
  let rec get_header acc =
    try
      let line = input_line ic in
      if line = "\r" || line = ""
      then acc
      else get_header(strip_cr line::acc)
    with End_of_file -> acc
  in
  let header = get_header []
  in
  let buf = Buffer.create 10240 in
  let tmp = String.make 1024 '\000' in
  let rec aux lim =
    let bytes = input ic tmp 0 (min lim 1024) in
    if bytes > 0 then begin
      Buffer.add_substring buf tmp 0 bytes;
      aux (lim - bytes)
    end
  in
  let rec aux_nolim() =
    let bytes = input ic tmp 0 1024 in
    if bytes > 0 then begin
      Buffer.add_substring buf tmp 0 bytes;
      aux_nolim()
    end
  in
  (try
     match limit with
     | Some lim -> aux lim
     | None -> aux_nolim()
   with End_of_file -> ());
  let page = Buffer.contents buf in
  (first_line, header, page)
;;

let cut_url ~url =
  let len = String.length url in
  let (address, len) =
    if len < 7 then (url, len) else
    begin
      let first_7 = String.sub url 0 7 in
      if first_7 = "http://"
      then (String.sub url 7 (len - 7), (len - 7))
      else (url, len)
    end
  in
  let (address, path) =
    try
      let pos = String.index address '/' in
      (String.sub address 0 pos,
       String.sub address (pos) (len - pos))
    with _ ->
      (address, "/")
  in
  (address, path)
;;

let make_request ~url ?(port=80) ?(kind=GET) ?referer ?user_agent () =
  let (address, path) = cut_url ~url in
  let (inchan, outchan) = submit_request ~address ~port ~kind ~path ~referer ~user_agent in
  let cont = cont_of_inchan inchan in
  close_in inchan;
  (cont)
;;

```

