BPSOSRX2 ;ALB/SS - ECME REQUESTS ;30-NOV-07
 ;;1.0;E CLAIMS MGMT ENGINE;**7,8**;JUN 2004;Build 29
 ;;Per VHA Directive 2004-038, this routine should not be modified.
 ;
 ;to store insurer data in BPS INSURER file
 ; RXI - RX ien 
 ; RXR - refill number
 ; MOREDATA - Array of data needed for transaction/claim
 ; BPCOBIND - "active" COB indicator (the one is processed currently) COB
 ; BPIEN77 - BPS REQUEST ien (request for which the BPS INSURER DATA record is created)
 ;
 ; 
INSURER(RXI,RXR,MOREDATA,BPCOBIND) ;
 N BPIEN78,BPIEN59
 ;IBDATA
 ;Create a new record with .01 field only
 S BPIEN59=$$IEN59^BPSOSRX(RXI,RXR,BPCOBIND)
 ;
 S BPIEN78=+$$INSITEM^BPSUTIL2(9002313.78,"",BPIEN59,"","") ;RX ien
 I BPIEN78<1 Q "0^Cannot create a record in BPS INSURER DATA"
 I $$FILLFLDS^BPSUTIL2(9002313.78,".07",BPIEN78,$P($G(MOREDATA("IBDATA",BPCOBIND,1)),U,14))
 I $$FILLFLDS^BPSUTIL2(9002313.78,".08",BPIEN78,$P($G(MOREDATA("IBDATA",BPCOBIND,1)),U,1))
 I $$FILLFLDS^BPSUTIL2(9002313.78,".09",BPIEN78,BPCOBIND)
 ;
 I $$FILLFLDS^BPSUTIL2(9002313.78,"1.01",BPIEN78,$P($G(MOREDATA("IBDATA",BPCOBIND,1)),U,2))
 I $$FILLFLDS^BPSUTIL2(9002313.78,"1.02",BPIEN78,$P($G(MOREDATA("IBDATA",BPCOBIND,1)),U,3))
 I $$FILLFLDS^BPSUTIL2(9002313.78,"1.03",BPIEN78,$P($G(MOREDATA("IBDATA",BPCOBIND,1)),U,5))
 I $P($G(MOREDATA("IBDATA",BPCOBIND,1)),U,6)'="" I $$FILLFLDS^BPSUTIL2(9002313.78,"1.04",BPIEN78,$P($G(MOREDATA("IBDATA",BPCOBIND,1)),U,6))
 I $$FILLFLDS^BPSUTIL2(9002313.78,"1.05",BPIEN78,$P($G(MOREDATA("IBDATA",BPCOBIND,1)),U,7))
 I $$FILLFLDS^BPSUTIL2(9002313.78,"1.06",BPIEN78,$P($G(MOREDATA("IBDATA",BPCOBIND,1)),U,8))
 I $$FILLFLDS^BPSUTIL2(9002313.78,"1.07",BPIEN78,$P($G(MOREDATA("IBDATA",BPCOBIND,1)),U,9))
 I $$FILLFLDS^BPSUTIL2(9002313.78,"1.08",BPIEN78,$P($G(MOREDATA("IBDATA",BPCOBIND,1)),U,10))
 ;
 I $$FILLFLDS^BPSUTIL2(9002313.78,"2.01",BPIEN78,$P($G(MOREDATA("IBDATA",BPCOBIND,2)),U,1))
 I $$FILLFLDS^BPSUTIL2(9002313.78,"2.02",BPIEN78,$P($G(MOREDATA("IBDATA",BPCOBIND,2)),U,2))
 I $$FILLFLDS^BPSUTIL2(9002313.78,"2.03",BPIEN78,$P($G(MOREDATA("IBDATA",BPCOBIND,2)),U,3))
 I $$FILLFLDS^BPSUTIL2(9002313.78,"2.04",BPIEN78,$P($G(MOREDATA("IBDATA",BPCOBIND,2)),U,4))
 I $$FILLFLDS^BPSUTIL2(9002313.78,"2.05",BPIEN78,$P($G(MOREDATA("IBDATA",BPCOBIND,2)),U,5))
 I $P($G(MOREDATA("IBDATA",BPCOBIND,1)),U,13)'="" I $$FILLFLDS^BPSUTIL2(9002313.78,"2.06",BPIEN78,$P($G(MOREDATA("IBDATA",BPCOBIND,1)),U,13))
 ;
 I $$FILLFLDS^BPSUTIL2(9002313.78,"3.01",BPIEN78,$P($G(MOREDATA("IBDATA",BPCOBIND,3)),U,1))
 I $P($G(MOREDATA("IBDATA",BPCOBIND,3)),U,2)'="" I $$FILLFLDS^BPSUTIL2(9002313.78,"3.02",BPIEN78,$P($G(MOREDATA("IBDATA",BPCOBIND,3)),U,2))
 I $$FILLFLDS^BPSUTIL2(9002313.78,"3.03",BPIEN78,$P($G(MOREDATA("IBDATA",BPCOBIND,3)),U,3))
 I $$FILLFLDS^BPSUTIL2(9002313.78,"3.04",BPIEN78,$P($G(MOREDATA("IBDATA",BPCOBIND,3)),U,4))
 I $$FILLFLDS^BPSUTIL2(9002313.78,"3.05",BPIEN78,$P($G(MOREDATA("IBDATA",BPCOBIND,3)),U,5))
 I $$FILLFLDS^BPSUTIL2(9002313.78,"3.06",BPIEN78,$P($G(MOREDATA("IBDATA",BPCOBIND,3)),U,6))
 ;
 I $$FILLFLDS^BPSUTIL2(9002313.78,"4.01",BPIEN78,$P($G(MOREDATA("IBDATA",BPCOBIND,1)),U,4))
 I $P($G(MOREDATA("IBDATA",BPCOBIND,1)),U,11)'="" I $$FILLFLDS^BPSUTIL2(9002313.78,"4.02",BPIEN78,$P($G(MOREDATA("IBDATA",BPCOBIND,1)),U,11))
 I $P($G(MOREDATA("IBDATA",BPCOBIND,1)),U,12)'="" I $$FILLFLDS^BPSUTIL2(9002313.78,"4.03",BPIEN78,$P($G(MOREDATA("IBDATA",BPCOBIND,1)),U,12))
 ;
 I $$FILLFLDS^BPSUTIL2(9002313.78,"5.01",BPIEN78,+DUZ)
 I $$FILLFLDS^BPSUTIL2(9002313.78,"5.02",BPIEN78,DT)
 ;
 Q "1^"_BPIEN78
 ;
ERRFIELD(BP78,BPFIELD) ;
 N DIK,DA
 S DIK="^BPS(9002313.78,"
 S DA=BP78
 D ^DIK ;delete incomplete record
 ;return the error message
 Q $$FIELDMSG(0,"",9002313.78,$G(BPFIELD))
 ;
 ;store MOREDATE("IBDATA") in IB INSURER DATA
 ;  RXI - Prescription IEN
 ;  RXR - Fill Number
 ;  MOREDATA - Array of data needed for transaction/claim
 ;  BPINSUR(COB,IEN78) = array to return back BPS INSURERE DATA iens created 
 ;  return value:
 ;  1 = success
 ;  0^message = if one of the records wasn't created
MKINSUR(RXI,RXR,MOREDATA,BPINSUR) ;
 ;store MOREDATE("IBDATA") in IB INSURER DATA
 N BPQ,BPCOB,BPERRMSG
 S BPERRMSG=""
 S BPQ=0,BPCOB=0
 F  S BPCOB=$O(MOREDATA("IBDATA",BPCOB)) Q:+BPCOB=0!(BPQ=1)  D
 . S BPIEN78=$$INSURER(RXI,RXR,.MOREDATA,BPCOB)
 . I BPIEN78<1 S BPERRMSG="Missing data for the file #9002313.78, "_$P(BPIEN78,U,2),BPQ=1 Q
 . S BPINSUR(BPCOB)=+$P(BPIEN78,U,2)
 I BPQ=1 Q "0^"_BPERRMSG
 Q 1
 ;add field name to the message
 ;BPRFILE - if 1 then add file # to the message
 ;BPMESS,BPFILENO,BPFLDNO - message text, file # and field #
FIELDMSG(BPRFILE,BPMESS,BPFILENO,BPFLDNO) ;
 N BPFLDNM
 I ('$G(BPFILENO))!('$G(BPFLDNO)) Q $G(BPMESS)
 D FIELD^DID(BPFILENO,BPFLDNO,"","LABEL","BPFLDNM")
 Q $G(BPMESS)_$S($G(BPRFILE)=1:"file #"_BPFILENO_",",1:"")_"field #"_BPFLDNO_"("_$G(BPFLDNM("LABEL"))_")"
 ;BPSOSRX2
