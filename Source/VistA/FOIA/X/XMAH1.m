XMAH1 ;ISC-SF/GMB-Reply to a message API ;04/17/2002  07:33
 ;;8.0;MailMan;;Jun 28, 2002
 ; Was (WASH ISC)/CAP
 ;
 ; Entry points (DBIA 1232):
 ; ^XMAH1      Interactive respond to a message
 ; ENTA^XMAH1  Interactive respond to a message
 ;
ENTA ; Interactive respond to a message
 ; Needs:
 ; XMDUZ  user ID
 ; XMK    basket number
 ; XMZ    message number
 ; Optional:
 ; XMDF   If $D(XMDF), then addressing restrictions are ignored.
 N XMV,XMSUBJ,XMFROM,XMINSTR,XMKN,XMIEN,XMRESP,XMRESPSO,XMPAKMAN,XMSECURE,XMABORT
 S XMABORT=0
 D INIT(XMDUZ,XMK,.XMKN,XMZ,.XMSUBJ,.XMFROM,.XMINSTR,.XMIEN,.XMRESPSO,.XMRESP,.XMABORT) Q:XMABORT
 D REPLY^XMJMR(XMDUZ,.XMK,.XMKN,XMZ,XMSUBJ,XMFROM,.XMINSTR,XMIEN,XMRESPSO,0,XMRESP)
 Q
INIT(XMDUZ,XMK,XMKN,XMZ,XMSUBJ,XMFROM,XMINSTR,XMIEN,XMRESPSO,XMRESP,XMABORT) ;
 N XMIM,XMIU,XMZREC
 D INITAPI^XMVVITAE
 K XMERR,^TMP("XMERR",$J)
 S XMZREC=^XMB(3.9,XMZ,0)
 I '$$REPLY^XMXSEC(XMDUZ,XMZ,XMZREC) D SHOW^XMJERR S XMABORT=1 Q
 I $D(XMDF) S XMINSTR("ADDR FLAGS")="R" ; no addressing restrictions
 D INMSG^XMXUTIL2(XMDUZ,0,XMZ,XMZREC,"I",.XMIM,.XMINSTR,.XMIU)
 I $D(XMINSTR("SCR HINT")),'$D(XMSECURE),'$$KEYOK^XMJMCODE(XMZ,XMINSTR("SCR HINT")) S XMABORT=1 Q
 S XMSUBJ=XMIM("SUBJ")
 S XMFROM=XMIM("FROM")
 S XMRESPSO=XMIM("RESPS")
 S XMIEN=XMIU("IEN")
 S XMRESP=XMIU("RESP")
 S XMPAKMAN=$S(XMINSTR("TYPE")["X":1,XMINSTR("TYPE")["K":1,1:0)
 S XMKN=$P(^XMB(3.7,XMDUZ,2,XMK,0),U,1)
 Q
