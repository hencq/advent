#lang racket
(require rackunit
         threading)

(define (count-letters string)
  (for/fold [(counts (hash))]
            [(ch (in-string string))]
    (hash-update counts ch add1 0)))

(define (letter-count? n string)
  (define counts (hash-values (count-letters string)))
  (ormap (lambda (v) (= v n)) counts))

(module+ test
  (check-eq? (letter-count? 2 "abcdef") #f)
  (check-eq? (letter-count? 3 "abcdef") #f)
  
  (check-eq? (letter-count? 2 "bababc") #t)
  (check-eq? (letter-count? 3 "bababc") #t)

  (check-eq? (letter-count? 2 "abcccd") #f)
  (check-eq? (letter-count? 3 "abcccd") #t)
  )

(define (checksum strings)
  (define twos (length (filter (curry letter-count? 2) strings)))
  (define threes (length (filter (curry letter-count? 3) strings)))
  (* twos threes))

(module+ test
  (define ids (list
               "abcdef"
               "bababc"
               "abbcde"
               "abcccd"
               "aabcdd"
               "abcdee"
               "ababab"))
  (check-eq? (checksum ids) 12))

(define boxes (list
               "oeambtcgjqnzhgkdylfapoiusr"
               "oewmbtcxjqnzhgvdyltapvqusr"
               "oewmbtchjqnzigkdylfapviuse"
               "oeimbucxjqnzhgkdyyfapviusr"
               "fewmbtcxjqndhgcdylfapviusr"
               "oevgbtccjqnzhgkdylfapviusr"
               "oewmbtcxjqnzhnkdylmapvpusr"
               "oewmbtcxjqnzhxkdyldapvirsr"
               "oewmutccjqnzngkdylfapviusr"
               "oewmbtcxbqnzhgkdsliapviusr"
               "ozwmbtfxjqnzcgkdylfapviusr"
               "oewmbtdxjqnzhgkdypfapsiusr"
               "oeylbtcxjqnzhgyyylfapviusr"
               "oewmbtcxjqnzhgkdrlfakuiusr"
               "oewmbtcujqnxhgkdylfadviusr"
               "oewmbtcxlqpzhgkdylfaaviusr"
               "oewmztcxjqnzhgkdylfqpliusr"
               "oeembtcxjqnzhgkdtlmapviusr"
               "onwmbtcxjqnqhgkdylfapdiusr"
               "oewmbtcxnqnzhgkdylfapsbusr"
               "oeoibtjxjqnzhgkdylfapviusr"
               "oxwmbtcxjqnzhgkdylfapcipsr"
               "oewmbtoxbqnzhgzdylfapviusr"
               "okwubtcxjqnzhgkdylfapiiusr"
               "oewmbtcxjqnzhgodylfapnicsr"
               "oewmitcxjqnzhgkdylfaphlusr"
               "oewmbtaxjqnzhgkhylfapveusr"
               "oewmftcbjqnzhgkdylfapviurr"
               "oewmbtcujqnzbgkdylfapliusr"
               "oeevbtcxjqnzhgkdylfapniusr"
               "oewmbtcxjqnvhgkdylfapnpusr"
               "oewabtcxjqnzhgddylfapviust"
               "oewmbtyxjqnzhgkdvlfapvinsr"
               "jewmbtcxjonzhzkdylfapviusr"
               "oewmbrcxjqnzxgkdylfapoiusr"
               "dewmbtmxjqnzhgkdyvfapviusr"
               "oewmbtctjqnzhgkdmlfapvihsr"
               "oewmbjcxjqnzhgvdylfapviurr"
               "oewmbtcxjqnzhgcdxlfapvfusr"
               "oewmbucxjqnzhgkdyltapvifsr"
               "gewmbtcejqezhgkdylfapviusr"
               "oewebtcxjznzhgkdylfapvhusr"
               "oewmjtcxjqnzhgkdycfakviusr"
               "oewmbtcxjtnvhgkdylfabviusr"
               "oewmbtcxjqnthgkgclfapviusr"
               "hewmbtcxjqnzhgkdwlfapziusr"
               "oewmbtcxjqnzhgkdylfqpviysf"
               "oewmbtcxjvnzhgmdylfapviuse"
               "oewmbtcxjqnphgkdymzapviusr"
               "oewmbtcxjqnzwmkdylfapvbusr"
               "oewmbthxjqnzhgkdylfatvilsr"
               "oewmbtcxaznzhgkhylfapviusr"
               "zewmbscxjqnzhgkdylfatviusr"
               "oewmbecyaqnzhgkdylfapviusr"
               "oewmbtnxjqnzhekxylfapviusr"
               "oewmbtcxjqczhgkdyltnpviusr"
               "yewmbecxjnnzhgkdylfapviusr"
               "oewmbocxjqnzhgkyylfapviusv"
               "oewmxtcxjqnzhgkkylfspviusr"
               "oiwmbtcxjqnzhgkdydfapvgusr"
               "oewmbtcxjqnzngydylfwpviusr"
               "oewmctcxjqnzhgkdelfapviasr"
               "oewmbtcxjqnzhgxdwmfapviusr"
               "oewmntcxjqnzhgkdylfamviusw"
               "oewmatcxjqbzhgkdylfapvhusr"
               "oewmbtcxjqnqhmkdyluapviusr"
               "opwmbtcxjqnzhgkdywfapvilsr"
               "omwmbtcxjqnlhgkdylyapviusr"
               "oewmltcxoqnzhgkdylfapvfusr"
               "oewmbtcxjqtzhgkdyyoapviusr"
               "oewmbtcxjqnzhrkdzlffpviusr"
               "oewmbtqxyqnzhgkdylfalviusr"
               "oeuzbtcxlqnzhgkdylfapviusr"
               "oewmbtcxjqnzhtxdylflpviusr"
               "oewmdtfxjqnzhgkdylfapviufr"
               "ojwmbtcxjqnzhgkdylfypviqsr"
               "oewmbtcxjqnzhgkdylfapvivuf"
               "oewmjtcsjqnzxgkdylfapviusr"
               "ohembtcxjqnzhnkdylfapviusr"
               "oewmptcajqnzhgkdylfapviusd"
               "oewmbtcxjcnwhgkbylfapviusr"
               "oewmbtcxjqnzhgddnlfapvqusr"
               "oewmbtcfjqnzhgkdypfapvzusr"
               "oewdbtccjqnzhgfdylfapviusr"
               "oewmbtcxjpnzhgkdplfaqviusr"
               "oepmbhcxjqnzhgkdylfaaviusr"
               "oewmbtcwjqxzhgkwylfapviusr"
               "oewmatcxjqnchgkdylfapvifsr"
               "omwmbncxjqnzhgkdylfapviuyr"
               "sewmbsckjqnzhgkdylfapviusr"
               "oewubtcxjqnzhgkdyluapvausr"
               "ohwmbtcxqqhzhgkdylfapviusr"
               "oewmbtcxjqnzhgkpylfapnissr"
               "eewmbccxjqnzhgkdylbapviusr"
               "oewmitcyjqnzhgkdylkapviusr"
               "oewmbtcxjvnzhgkdyjfvpviusr"
               "oewmbtcxjqmzhgkdyefagviusr"
               "oewmbtcvjqnzhgkdylpapviufr"
               "oewmbtcxjrnkhgkdylfapsiusr"
               "oewmbtcxjqnzygkdylfaxvipsr"
               "oexmbtcxjqczhgkdyloapviusr"
               "oewmbtcxjqnlhtkdylfapvmusr"
               "oewmbtcxdqjzdgkdylfapviusr"
               "oewmbtclgqnzhgkdylfabviusr"
               "oewmbtvfjqnzhgkdylfapviulr"
               "oewmbtcxjqnzhgkdyllarvijsr"
               "oewmbtyxjqnzhgpdylxapviusr"
               "oeylbtcxjqnzhgkyylfapviusr"
               "oewmbtctjqnzhjkdylfapviulr"
               "oermatcxjqnzhgkdylzapviusr"
               "oewmbtcxjqnztgkdzlfapviutr"
               "oewlbtcxjqnztgkvylfapviusr"
               "oewmbtcxjqzvhgkdylfapviusk"
               "oewmbtcxjqnzmgkdyldapvilsr"
               "felmbtcxjqnzhgkdylfapviusl"
               "oewmbtcxjgnzhgkjylfaeviusr"
               "ovwmbtcxjqpzhgkdylfapvizsr"
               "eewmbtcpjqnzhgkdylfapvijsr"
               "oewmbzcxjqnzhgkdylfaeviutr"
               "tewmbtcljqhzhgkdylfapviusr"
               "oewmbtcujqnzhgkdnliapviusr"
               "oewmbtcljqnzhskdylfapvgusr"
               "oewmbtchjqnzhgkdylmapviuse"
               "oewmbtcxjqnzbgkdylfaiviurr"
               "oewmbtcxjqnzhkkdyloapsiusr"
               "oewjbtcxjqnhhgkdylfapjiusr"
               "odwmbtcnjqnzhgkdylfapvicsr"
               "oewmbccxjqrzwgkdylfapviusr"
               "kewmbtcvjqnzhgkdylaapviusr"
               "okwmbtcxjqnzhgkdylfspvausr"
               "oewmbtcxjynzhgkdyafapviusw"
               "oewmbtcxjqnzhgwdyleayviusr"
               "oewmbtcxjqnzhgkdylfapviicl"
               "oewmbtcxjqnzhgkdyltaeziusr"
               "oewmbtcxrqnzhgkdylftpvizsr"
               "oewsrtcgjqnzhgkdylfapviusr"
               "oewmbtsxgqnzhgxdylfapviusr"
               "oewmbtcxjanzhgtdylfapeiusr"
               "oewybtcgjqnzhgkdylfapviust"
               "ojwmbncxjqnzhgkdylfapgiusr"
               "ocgebtcxjqnzhgkdylfapviusr"
               "oejcbtcxjqnzhgkvylfapviusr"
               "oswmbtcxjqnkhgkdylfapviusb"
               "oewdbtcxjqnzdgkdylfypviusr"
               "oawmutcxjqnzhgkddlfapviusr"
               "oewzbtcxyqnzhgkdylfapviusy"
               "zewmbtcxjqnzkgkdylwapviusr"
               "aewmbtkxjqnzhgkdylfapviuer"
               "oewmbtcxwqnzhgkdyofapviuur"
               "oewmbtcxjqnzggkdylfapanusr"
               "oewmstcxuqnzhgkdylzapviusr"
               "zewmbtcxjqozhgkdelfapviusr"
               "oewzbtcxjqnahgkdyllapviusr"
               "fewmatcxjqnghgkdylfapviusr"
               "oewmbtcxjqnzhgkdylfapviyqb"
               "oewwbtcxjqnzhgkdyljapviqsr"
               "oewmbtbxjqnzhgkxylfapviesr"
               "oewmbtcbjqnphgkdylfapviysr"
               "oewabtcxyqnzhgkdylfabviusr"
               "oewmbtcxhknzhgkdylfapviusd"
               "ozwmbtcljqnzhgkdylfapviksr"
               "tewmbtcxjqnzhgkdylfaxvqusr"
               "oewmbtcxrqnzhgkdytfapvrusr"
               "ohwmbtcxjcnzhgkdyifapviusr"
               "oewmbpcxjqnzhwkdylfaphiusr"
               "oedmbtcxjqnzhgnbylfapviusr"
               "oewmbocxjqnehgkdylfapvbusr"
               "oeymbtcxjqezegkdylfapviusr"
               "oewmbtcxjqnzhgkdllferviusr"
               "oewmbtcxjqnzhgkwmlfawviusr"
               "oewmbtcxienzhgkdyzfapviusr"
               "mewmbtcxjqnzhqkdylfapviwsr"
               "oewmbtcxjqnztgkmylfapvdusr"
               "ouwmbtcxjqnzhokdylpapviusr"
               "oewmctcxjqhzhgmdylfapviusr"
               "oewmbtcyjqnzhmkdylfarviusr"
               "oewmbtcxjqnzhgkdpnfzpviusr"
               "oewmptcxjqnzhgkdylkapviulr"
               "nefmbtcxsqnzhgkdylfapviusr"
               "oewmbtcxwqnzhgkdilfapvizsr"
               "eewmbtcxjqwzhghdylfapviusr"
               "oewmbtixmqnzhgkjylfapviusr"
               "okwmbtcdzqnzhgkdylfapviusr"
               "oewmbtxxjrnzigkdylfapviusr"
               "oewmdycxjqnzhekdylfapviusr"
               "oewmutcxjqnzhgkdylfapsiuqr"
               "oewmbacxjqnzrgkdrlfapviusr"
               "oewmbtcxpqnzhmkdylfapciusr"
               "oewabtcxjqnzhgkdyrcapviusr"
               "oswmbtcxjqnzhgkdrxfapviusr"
               "gewmbtcnjqnzhgkdylvapviusr"
               "newmbtcxjwnzfgkdylfapviusr"
               "lewmbtcxjqnzhgkdylfaptiujr"
               "oewwbtcxjqndhgkdylfapiiusr"
               "oewmbtcxjqnzhggdylfapvqmsr"
               "lewmbtcxjqnzhgkhllfapviusr"
               "oewmbtocjqnzhgkdylfapvhusr"
               "oedmbtcxjqnzhgkdyhfapviusb"
               "oewmbtcxjqnzhgkdylfajvaosr"
               "zewmbtcxjqnzhgkdylfapvsssr"
               "oewmbthxjqnzhskdylfapviuqr"
               "yewmrtcvjqnzhgkdylfapviusr"
               "oewmbtctjqnzhgkdylfabvhusr"
               "oesmstcxjqnzhgkdylfapqiusr"
               "oewmbtcxjqnzzgkdylfopiiusr"
               "otwmbtzxjqnzhgkdylfaxviusr"
               "ouwmbxcxjqnzhgkdylfapvnusr"
               "oewmbtcxjqezhgedylfapvsusr"
               "oesmhtcsjqnzhgkdylfapviusr"
               "oewdbtcxjqnzhgkdilfapvifsr"
               "oewmbtcxjqnzhgudynfamviusr"
               "qewhbtcxjqnzhgkdyxfapviusr"
               "oewmbzcxjqtzhgkdylfapvitsr"
               "oewmbtccjqzzhgkaylfapviusr"
               "jewmbtcxmqnzhlkdylfapviusr"
               "oewmbtcxjqbzhgkdnlfapviusp"
               "oeimbtcdjqnzhgkdylfapviuer"
               "oewtbtcxjqnihgkdylfahviusr"
               "oewmbtcxhqnzhgkdylfapdiudr"
               "oefmbtcxjqyshgkdylfapviusr"
               "oewmbtcxjqnzhgkfglfapviusx"
               "oecmbocxjqnzhgkdmlfapviusr"
               "oewmbtcxjqnzhghdylfavviuhr"
               "oewmbmcxiqnzhgkpylfapviusr"
               "oewmbtcnjqnzhgkrylfanviusr"
               "oewmbocxjqnzhzkdllfapviusr"
               "eewmbtckjqnzhgkdylfapviusg"
               "oewmbtcrjqqzhgkdylfapvigsr"
               "oewmbtcxjqazhgfdylfapjiusr"
               "oetmbtcxjqnzhgldylfapviqsr"
               "oewbbtcxjqnzlgkdylfapviuse"
               "oewmbtcxjqnzhglbolfapviusr"
               "oewmbtcxjqnzcgkdylfapviuhy"
               "oelmbtcxjqfzhgkdylaapviusr"
               "oojmbycxjqnzhgkdylfapviusr"
               "oewmbtrxjqnrhgkdylfapniusr"
               "oewmbtcxjqnzhgkyyhfapviuso"
               "oewabtcxjqnzhskdylfapviusx"
               "oewmbtcrjqnmhgkdylfapvnusr"
               "oewmbtcxjqrzhgkdylfapvpuss"
               "oewmbtcxhqnzwgkddlfapviusr"
               "kewmbtcxjqnzhgkyylfajviusr"
               "oswmbtcxjqnzhgkdjlfapviuss"
               "onwmbtcxjqnchgkdylfapvpusr"
               "oeymbtcxjqnxhikdylfapviusr"
               "oewmblcdjqnzhgkdylfapviysr"
               "oewmbtcxeqczhgudylfapviusr"
               "oewmbpgxjqnzhgkdylfapfiusr"
               "ohwmwtcxjqnzhgkdylftpviusr"
               "zebmbtuxjqnzhgkdylfapviusr"))

(checksum boxes)


(define (str-diff s1 s2)
  (for/fold [(count 0)]
            [(c1 s1)
             (c2 s2)]
    (if (eq? c1 c2)
        count
        (add1 count))))

(define (common-chars s1 s2)
  (list->string 
   (for/list ((c1 s1)
              (c2 s2)
              #:when (eq? c1 c2))
     c1)))


(define (find-boxes boxes)
  (define combos (cartesian-product boxes boxes))
  (~>> combos
       (filter (lambda (combo) (= (apply str-diff combo) 1)))
       (map (lambda (combo) (apply common-chars combo)))))

  
(module+ test
  (define ids2 (list
             "abcde"
             "fghij"
             "klmno"
             "pqrst"
             "fguij"
             "axcye"
             "wvxyz"))
  (check-eq? (car (find-boxes ids)) "fgij"))
