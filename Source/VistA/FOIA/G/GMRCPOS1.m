GMRCPOS1 ;SLC/DCM - Post init to move Services from file 123.5 to the orderable items file, 101.43, and orderables in file 101 to file 101.43 ;10/28/98  14:31
 ;;3.0;CONSULT/REQUEST TRACKING;**1**;DEC 27, 1997
EN ;Entry point
 D SVC,ITEMS
 Q
SVC ;;get services from file 123.5, format them into an HL-7 message and send the message to OE/RR to update file 101.43
 S GMRCSSN="",GMRCSRN=0,RLEVCODE="MAD"
 F  S GMRCSSN=$O(^GMR(123.5,"B",GMRCSSN)) Q:GMRCSSN=""  S GMRCSRN=$O(^GMR(123.5,"B",GMRCSSN,0)) I $P(^GMR(123.5,GMRCSRN,0),"^",2)'=1,GMRCSSN'="ALL SERVICES" D
 .I $P(^GMR(123.5,GMRCSRN,0),"^",2)=9 S GMRCDC(GMRCSSN)=GMRCSRN
 .D SVC^GMRCPOST(GMRCSRN,GMRCSSN,RLEVCODE)
 .D MSG^XQOR("GMRC ORDERABLE ITEM UPDATE",.GMRCMSG)
 .K GMRCMSG
 .Q
 S GMRCSSN="",GMRCSRN=0,RLEVCODE="MDC"
 F  S GMRCSSN=$O(GMRCDC(GMRCSSN)) Q:GMRCSSN=""  S GMRCSRN=GMRCDC(GMRCSSN) D
 .D SVC^GMRCPOST(GMRCSRN,GMRCSSN,RLEVCODE)
 .D MSG^XQOR("GMRC ORDERABLE ITEM UPDATE",.GMRCMSG)
 .K GMRCMSG
 .Q
 K GMRCLCN,GMRCLSSN,GMRCSS,GMRCSSN,GMRCSRN,I,RLEVCODE,X
 Q
ITEMS ;create an HL-7 message to move orderable items into the orderable items file - file 123.43 - and send it to oerr
 S GMRCMSTP="MFN",GMRCRELC="MAD"
 F PFX="GMRCR " D  ;,"GMRCT " D
 .S GMRCPFX=PFX F  S GMRCPFX=$O(^ORD(101,"B",GMRCPFX)) Q:$E(GMRCPFX,1,$L(PFX))'=PFX  S ORIEN=$O(^ORD(101,"B",GMRCPFX,0)) D  K GMRCARRY,GMRCMSG
 ..S GMRCPF1=$E(GMRCPFX,$L(PFX)+1,$L(GMRCPFX))
 ..Q:$E(GMRCPF1,1,6)="SAMPLE"
 ..S NO=0,NO1=1 F  S NO=$O(^ORD(101,ORIEN,2,NO)) Q:NO?1A.E!(NO="")  S SYN=^ORD(101,ORIEN,2,NO,0),GMRCARRY(NO1)=SYN,NO1=NO1+1
 ..D ITEMS^GMRCPOST(GMRCMSTP,GMRCRELC,ORIEN,GMRCPF1,.GMRCARRY,PFX)
 ..D MSG^XQOR("GMRC ORDERABLE ITEM UPDATE",.GMRCMSG)
 ..Q
 .Q
 K GMRCMSTP,GMRCPF1,GMRCPFX,GMRCRELC,NO,NO1,ORIEN,PFX,SYN
 Q
