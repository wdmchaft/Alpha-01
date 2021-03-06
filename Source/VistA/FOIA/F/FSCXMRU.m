FSCXMRU ;SLC/STAFF-NOIS Xrefs MRU ;12/27/96  14:25
 ;;1.1;NOIS;;Sep 06, 1998
 ;
AUTL(OP,FIELD,VALUE,NUM) ; from dd 7101.1
 N USER,TIME,LIST,INDEX
 I OP="SET" D  Q
 .I FIELD="USER" D  Q
 ..I VALUE S TIME=$P(^FSCD("MRU",NUM,0),U,2),LIST=$P(^(0),U,3),INDEX=+$P(^(0),U,4) I TIME,LIST S ^FSCD("MRU","AUTL",VALUE,-TIME,LIST,INDEX)=NUM
 .I FIELD="TIME" D  Q
 ..I VALUE S USER=$P(^FSCD("MRU",NUM,0),U),LIST=$P(^(0),U,3),INDEX=+$P(^(0),U,4) I USER,LIST S ^FSCD("MRU","AUTL",USER,-VALUE,LIST,INDEX)=NUM
 .I FIELD="LIST" D  Q
 ..I VALUE S USER=$P(^FSCD("MRU",NUM,0),U),TIME=$P(^(0),U,2),INDEX=+$P(^(0),U,4) I USER,TIME S ^FSCD("MRU","AUTL",USER,-TIME,VALUE,INDEX)=NUM
 .I FIELD="VALUE" D  Q
 ..S USER=$P(^FSCD("MRU",NUM,0),U),TIME=$P(^(0),U,2),LIST=$P(^(0),U,3) I USER,TIME,LIST S ^FSCD("MRU","AUTL",USER,-TIME,LIST,+VALUE)=NUM
 I OP="KILL" D  Q
 .I FIELD="USER" D  Q
 ..I VALUE S TIME=$P(^FSCD("MRU",NUM,0),U,2),LIST=$P(^(0),U,3),INDEX=+$P(^(0),U,4) I TIME,LIST K ^FSCD("MRU","AUTL",VALUE,-TIME,LIST,INDEX)
 .I FIELD="TIME" D  Q
 ..I VALUE S USER=$P(^FSCD("MRU",NUM,0),U),LIST=$P(^(0),U,3),INDEX=+$P(^(0),U,4) I USER,LIST K ^FSCD("MRU","AUTL",USER,-VALUE,LIST,INDEX)
 .I FIELD="LIST" D  Q
 ..I VALUE S USER=$P(^FSCD("MRU",NUM,0),U),TIME=$P(^(0),U,2),INDEX=+$P(^(0),U,4) I USER,TIME K ^FSCD("MRU","AUTL",USER,-TIME,VALUE,INDEX)
 .I FIELD="VALUE" D  Q
 ..S USER=$P(^FSCD("MRU",NUM,0),U),TIME=$P(^(0),U,2),LIST=$P(^(0),U,3) I USER,TIME,LIST K ^FSCD("MRU","AUTL",USER,-TIME,LIST,+VALUE)
 Q
 ;
AUL(OP,FIELD,VALUE,NUM) ; from dd 7101.1
 N USER,LIST
 I OP="SET" D  Q
 .I FIELD="USER" D  Q
 ..I VALUE S LIST=$P(^FSCD("MRU",NUM,0),U,3),INDEX=+$P(^(0),U,4) I LIST S ^FSCD("MRU","AUL",VALUE,LIST,INDEX)=NUM
 .I FIELD="LIST" D  Q
 ..I VALUE S USER=$P(^FSCD("MRU",NUM,0),U),INDEX=+$P(^(0),U,4) I USER S ^FSCD("MRU","AUL",USER,VALUE,INDEX)=NUM
 .I FIELD="VALUE" D  Q
 ..S USER=$P(^FSCD("MRU",NUM,0),U),LIST=$P(^(0),U,3) I USER,LIST S ^FSCD("MRU","AUL",USER,LIST,+VALUE)=NUM
 I OP="KILL" D  Q
 .I FIELD="USER" D  Q
 ..I VALUE S LIST=$P(^FSCD("MRU",NUM,0),U,3),INDEX=+$P(^(0),U,4) I LIST K ^FSCD("MRU","AUL",VALUE,LIST,INDEX)
 .I FIELD="LIST" D  Q
 ..I VALUE S USER=$P(^FSCD("MRU",NUM,0),U),INDEX=+$P(^(0),U,4) I USER K ^FSCD("MRU","AUL",USER,VALUE,INDEX)
 .I FIELD="VALUE" D  Q
 ..S USER=$P(^FSCD("MRU",NUM,0),U),LIST=$P(^(0),U,3) I USER,LIST K ^FSCD("MRU","AUL",USER,LIST,+VALUE)
 Q
 ;
AUT(OP,FIELD,VALUE,NUM) ; from dd 7101.1
 N USER,TIME
 I OP="SET" D  Q
 .I FIELD="USER" D  Q
 ..I VALUE S TIME=$P(^FSCD("MRU",NUM,0),U,2) I TIME S ^FSCD("MRU","AUT",VALUE)=TIME
 .I FIELD="TIME" D  Q
 ..I VALUE S USER=$P(^FSCD("MRU",NUM,0),U) I USER S ^FSCD("MRU","AUT",USER)=VALUE
 I OP="KILL" D  Q
 .I FIELD="USER" D  Q
 ..I VALUE K ^FSCD("MRU","AUT",VALUE)
 .I FIELD="TIME" D  Q
 ..S USER=$P(^FSCD("MRU",NUM,0),U) I USER K ^FSCD("MRU","AUT",USER)
 Q
