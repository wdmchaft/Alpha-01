VAQBUL05 ;ALB/JRP - BULLETINS;2-JUNE-93
 ;;1.5;PATIENT DATA EXCHANGE;;NOV 17, 1993
XMITERR ;SENDS BULLETIN FOR MESSAGE RECEIVING ERRORS
 ;  DECLARATIONS DONE IN SERVER^VAQADM2
 S LINE=1
 S @XMIT@(LINE,0)="The following error(s) occurred while receiving a PDX transmission ..."
 S LINE=LINE+1
 F TMP=1:1:2 S @XMIT@(LINE,0)="",LINE=LINE+1
 ;PUT IN GENERAL INFO
 S TMP="General Information"
 S @XMIT@(LINE,0)=TMP
 S LINE=LINE+1
 S TMP=$$REPEAT^VAQUTL1("-",19)
 S @XMIT@(LINE,0)=TMP
 S LINE=LINE+1
 S TMP="XMZ: "_$G(XMZ)
 S @XMIT@(LINE,0)=TMP
 S LINE=LINE+1
 S TMP="Global Location: ^XMB(3.9,"_$G(XMZ)_")"
 S @XMIT@(LINE,0)=TMP
 S LINE=LINE+1
 S TMP="Postmaster Basket: S.VAQ-PDX-SERVER"
 S @XMIT@(LINE,0)=TMP
 S LINE=LINE+1
 S TMP="Sent By: "_$G(XMFROM)
 S @XMIT@(LINE,0)=TMP
 S LINE=LINE+1
 F TMP=1:1:2 S @XMIT@(LINE,0)="",LINE=LINE+1
 ;PUT IN GENERAL ERROR INFO
 S TMP="General Error"
 S @XMIT@(LINE,0)=TMP
 S LINE=LINE+1
 S TMP=$$REPEAT^VAQUTL1("-",13)
 S @XMIT@(LINE,0)=TMP
 S LINE=LINE+1
 S XMPOS=LINE
 S TMP=""
 F  S TMP=+$O(@ERROR@("GENERAL",TMP)) Q:('TMP)  D
 .S @XMIT@(LINE,0)=$G(@ERROR@("GENERAL",TMP))
 .S LINE=LINE+1
 I (XMPOS=LINE) D
 .S TMP="Not applicable"
 .S @XMIT@(LINE,0)=TMP
 .S LINE=LINE+1
 F TMP=1:1:2 S @XMIT@(LINE,0)="",LINE=LINE+1
 ;PUT IN SPECIFIC ERROR INFO
 S TMP="Specific Error(s)"
 S @XMIT@(LINE,0)=TMP
 S LINE=LINE+1
 S TMP=$$REPEAT^VAQUTL1("-",17)
 S @XMIT@(LINE,0)=TMP
 S LINE=LINE+1
 S XMPOS=LINE
 S MESSAGE=""
 F  S MESSAGE=+$O(@ERROR@(MESSAGE)) Q:('MESSAGE)  D
 .S TMP="Message Number: "_MESSAGE
 .S @XMIT@(LINE,0)=TMP
 .S LINE=LINE+1
 .S TMP=""
 .F  S TMP=+$O(@ERROR@(MESSAGE,TMP)) Q:('TMP)  D
 ..S @XMIT@(LINE,0)=$G(@ERROR@(MESSAGE,TMP))
 ..S LINE=LINE+1
 .S TMP=""
 .S @XMIT@(LINE,0)=TMP
 .S LINE=LINE+1
 I (XMPOS=LINE) D
 .S TMP="Not applicable"
 .S @XMIT@(LINE,0)=TMP
 .S LINE=LINE+1
 F TMP=1:1:2 S @XMIT@(LINE,0)="",LINE=LINE+1
 ;PUT IN NOTE
 S TMP="Note: Each PDX transmission can contain more than one PDX message.  Message"
 S @XMIT@(LINE,0)=TMP
 S LINE=LINE+1
 S TMP="      number X refers to the Xth message within the transmission (not the"
 S @XMIT@(LINE,0)=TMP
 S LINE=LINE+1
 S TMP="      XMZ)."
 S @XMIT@(LINE,0)=TMP
 S LINE=LINE+1
 ;SEND BULLETIN
 S XMY("G.VAQ PDX ERRORS")=""
 S TYPE="UNABLE TO PROPERLY RECEIVE TRANSMISSION"
 S TMP="Patient Data eXchange"
 S XMER=$$SENDBULL^VAQBUL(TYPE,"PDX",TMP,XMIT)
 S:(XMER>0) XMER=0
 Q
