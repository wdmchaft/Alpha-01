DVBAADM1 ;ALB ISC/THM-CHECK FOR ADMISSION TO HOSPITAL ;21 JUL 89
 ;;2.7;AMIE;;Apr 10, 1995
 S DVBAX=0,DVBAX=$O(^DGPM("APTT1",DFN,DVBAX)) G INP:DVBAX'>0 S ADM=1 Q
 ;
INP K DVBAX D STATUS I '$D(ADM) Q
 Q
STATUS ;PATIENT STATUS
 S:'$D(DOCTYPE) DOCTYPE="A" I DOCTYPE="A",'$D(ADM) W *7,!!,"This patient has never been admitted.",!! K Z H 3 Q
 Q
