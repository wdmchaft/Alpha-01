PRCGPMK ;WIRMFO@ALTOONA/CTB/WIRMFO/PLT - IFCAP PURGEMASTER SUBMANAGER (KILLER) ;12/10/97  9:54 AM
V ;;5.1;IFCAP;;Oct 20, 2000
 ;Per VHA Directive 10-93-142, this routine should not be modified.
 S ZTREQ="@"
 FOR  DO  Q:$$STOP!END
 . S END=0
 . S NODE=$$NEXT
 . I NODE="" S END=1 QUIT
 . S DA=$P(NODE,"^"),ROU=$P(NODE,"^",2,3),VARIABLE=$P(NODE,"^",4)
 . I ROU=""!(ROU="^")!(ROU?.E1"^")!(DA="") QUIT
 . S X=$P(ROU,"^",2) X ^%ZOSF("TEST") E  QUIT
 . S XROU=ROU I VARIABLE]"" S XROU=ROU_"("_""""_VARIABLE_""""_")"
 . S IPDA=0 D ADDIP^PRCGPM1(ROU,VARIABLE,.IPDA)
 . D @XROU S X=$P(ROU,"^",2)
 . D REMIP^PRCGPM1(IPDA)
 . K NODE,XROU,ROU,DA,VARIABLE,IPDA
 . S END=0
 . QUIT
 QUIT
 ;
ERR ; REPORT ERRORS TO FILE
 QUIT
 ;
STOP() ;CHECK TIME
 N NOW
 S NOW=$H
 I +STOP>(+NOW) QUIT 0
 I +NOW=(+STOP),$P(NOW,",",2)<$P(STOP,",",2) QUIT 0
 QUIT 1
 ;
NEXT() ;GET NEXT NUMBER FROM 443.1
 ;EXTRINSIC FUNCTION TO RETURN NEXT AVAILABLE RECORD .
 NEW DA,I,NODE
XX S DA=0
 FOR  S DA=$O(^PRC(443.1,DA)) Q:'DA  L +^PRC(443.1,DA):4 I  Q
 I DA="" QUIT ""
 I $D(^PRC(443.1,DA,0))["0" DO  G XX
  . DO REMOVE(DA)
  . L -^PRC(443.1,DA)
  . QUIT
 S NODE=^PRC(443.1,DA,0)
 D REMOVE(DA)
 L -^PRC(443.1,DA)
 QUIT NODE
 ;
REMOVE(DA) ;REMOVE ENTRY FROM FILE 443.1
 ;PARAMETER CALL TO REMOVE RECORD 'DA' FROM FILE
  NEW NODE,LAST,TOTAL
  I +DA=0!(DA'=+DA) QUIT
  I '$D(^PRC(443.1,DA)) QUIT
  FOR  L +^PRC(443.1,0):1 I  Q
  S NODE=^PRC(443.1,0),LAST=$P(NODE,"^",3),TOTAL=$P(NODE,"^",4)
  K ^PRC(443.1,DA) S TOTAL=TOTAL-1
  I DA'<LAST F  S LAST=LAST-1 Q:($D(^PRC(443.1,LAST))!(LAST=0))
  S $P(^PRC(443.1,0),"^",3,4)=LAST_"^"_TOTAL
  I $O(^PRC(443.1,0))="" S $P(^(0),"^",3,4)="^"
  L -^PRC(443.1,0)
  QUIT
