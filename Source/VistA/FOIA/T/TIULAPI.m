TIULAPI ; SLC/JER - Extract selected documents from TIU ;6/7/06  11:14
 ;;1.0;TEXT INTEGRATION UTILITIES;**211**;Jun 20, 1997;Build 26
MAIN(DFN,TIUDOC,STATUS,TIME1,TIME2,OCCLIM,TEXT) ; Control branching
 N TIUDA,TIUDT,TIUPRM0,TIUPRM1,TIUPRM3,COUNT,TIUSI,TIUS,TIUTI,TYPES
 D SETPARM^TIULE
 S:+$G(OCCLIM)'>0 OCCLIM=999
 S:+$G(TIME1)'>0 TIME1=6666666
 S:+$G(TIME2)'>0 TIME2=9999999
 K ^TMP("TIU",$J)
 I '$D(TIUPRM0) D SETPARM^TIULE
 D DOCTYPE^TIUSRVL(.TYPES,TIUDOC)
 I $D(STATUS)'>9 D STATUS^TIUSRVL(.STATUS,$S($G(STATUS)]"":STATUS,1:"ALL"))
 S TIUTI=0 F  S TIUTI=$O(TYPES(TIUTI)) Q:+TIUTI'>0  D
 . S TIUDOC=+$G(TYPES(TIUTI))
 . S TIUSI=0 F  S TIUSI=$O(STATUS(TIUSI)) Q:+TIUSI'>0  D
 . . S TIUS=+$G(STATUS(TIUSI)),TIUDT=TIME1
 . . F  S TIUDT=$O(^TIU(8925,"APT",DFN,TIUDOC,TIUS,TIUDT)) Q:+TIUDT'>0!(TIUDT>TIME2)!(+$G(COUNT)'<OCCLIM)  D
 . . . S TIUDA=0 F  S TIUDA=$O(^TIU(8925,"APT",DFN,TIUDOC,TIUS,TIUDT,TIUDA)) Q:+TIUDA'>0  D
 . . . . I +$$ISADDNDM^TIULC1(TIUDA),+TEXT Q
 . . . . I +$$CANDO^TIULP(TIUDA,"VIEW")'>0 Q
 . . . . S COUNT=+$G(COUNT)+1
 . . . . D EXTRACT^TIULQ(TIUDA,"^TMP(""TIU"",$J,"_COUNT_")",.TIUERR,".01;.05;.07;.08;1202;1203;1205;1208;1209;1301;1307;1402;1501:1505;1507:1513;1701;89261","",1)
 Q