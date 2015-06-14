
(in-package :cl-user)
(defpackage :dns.server.resolver.stub-test
  (:use :cl
        :dns
        :dns.def.struct
        :prove))
(in-package :dns.server.resolver.stub-test)



(defvar *cache-server* 
  '("8.8.8.8"))

(defvar *names* 
  '("www.yahoo.co.jp" 
    "www.google.com" 
    "www.sie.dendai.ac.jp"
    "jprs.jp"
    "twitter.com"
    "www.facebook.com"
    "www.youtube.com"
    "moratori.myhome.cx"
    "www.soumu.go.jp"
    "www.msn.com"
    "github.com"
    "ja.wikipedia.org"
    "www.kantei.go.jp"
    "jprs.co.jp"
    "instagram.com"
    "www.amazon.co.jp"
    "www.amazon.com"
    "telracsmoratori.blog.fc2.com")) 


(defun enquire (domain server)
  (dns.server.resolver.stub::enquire 
    (make-instance 'dns.def.struct:a-record
                   :id (random 65535)
                   :rd 1
                   :name domain)
    server))



(plan (+ 0 (* (length *cache-server*) (length *names*)) ))


(subtest "Google public DNSに問い合わせて返答をエラー無くパースできるか"
  (loop 
  for server in *cache-server* 
  do 
  (loop 
    for name in *names* 
    do
    (sleep 0.15)
    (is-type (enquire name server) 'dns-packet))))


(finalize)


