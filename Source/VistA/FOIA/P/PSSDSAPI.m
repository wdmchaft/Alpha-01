PSSDSAPI ;BIR/RTR-Dose Check APIs routine ;06/09/08
 ;;1.0;PHARMACY DATA MANAGEMENT;**117**;9/30/97;Build 101
 ;
EXMT(PSSVLIEN) ;Test if Drug should have Dose Call performed on it
 ;PSSVLIEN=File 50 internal entry number 
 ;1=exempt from Dose Check, 0=not exempt from Dose check
 N PSSVLND,PSSVLND1,PSSVLND3,PSSVLNDF,PSSVLDOV,PSSVLZR,PSSVLDF
 S PSSVLZR=$G(^PSDRUG(PSSVLIEN,0))
 I $P(PSSVLZR,"^",3)["S"!($E($P(PSSVLZR,"^",2),1,2)="XA") Q 1
 S PSSVLND=$G(^PSDRUG(PSSVLIEN,"ND"))
 S PSSVLND1=$P(PSSVLND,"^"),PSSVLND3=$P(PSSVLND,"^",3)
 S PSSVLDOV="" K PSSVLDF
 I $T(OVRIDE^PSNAPIS)]"",PSSVLND1,PSSVLND3 S PSSVLDOV=$$OVRIDE^PSNAPIS(PSSVLND1,PSSVLND3)
 I PSSVLND1,PSSVLND3 S PSSVLNDF=$$DFSU^PSNAPIS(PSSVLND1,PSSVLND3) S PSSVLDF=$P(PSSVLNDF,"^")
 I $G(PSSVLDF)'>0,$P($G(^PSDRUG(PSSVLIEN,2)),"^") S PSSVLDF=$P($G(^PS(50.7,+$P($G(^PSDRUG(PSSVLIEN,2)),"^"),0)),"^",2)
 I PSSVLDOV=""!('$G(PSSVLDF))!($P($G(^PS(50.606,+$G(PSSVLDF),1)),"^")="") Q 0
 I $P($G(^PS(50.606,+$G(PSSVLDF),1)),"^"),'PSSVLDOV Q 1
 I '$P($G(^PS(50.606,+$G(PSSVLDF),1)),"^"),PSSVLDOV Q 1
 Q 0
 ;
 ;
SUP(PSSSPLIN) ;Screen for Drug Interaction and Duplicate Therapy
 ;PSSSPLIN = File 50 internal entry number
 ;1=exempt, 0=not exempt
 N PSSSPLZR
 S PSSSPLZR=$G(^PSDRUG(PSSSPLIN,0))
 I $P(PSSSPLZR,"^",3)["S"!($E($P(PSSSPLZR,"^",2),1,2)="XA") Q 1
 Q 0
 ;
 ;
MRT(PSSRS1) ;Return Standard Medication Route and First DataBank Route
 N PSSRS2,PSSRS3
 S PSSRS2=""
 I '$G(PSSRS1) G MRTX
 I $P($G(^PS(51.2,PSSRS1,0)),"^",4)'=1 G MRTX
 S PSSRS3=$P($G(^PS(51.2,PSSRS1,1)),"^") I '$G(PSSRS3) G MRTX
 I '$D(^PS(51.23,+PSSRS3,0)) G MRTX
 I $$SCREEN^XTID(51.23,.01,+PSSRS3_",") G MRTX
 S PSSRS2=$G(^PS(51.23,+PSSRS3,0))
MRTX ;
 Q $P(PSSRS2,"^")_"^"_$P(PSSRS2,"^",2)
 ;
 ;
UNIT(PSSVUTUN) ;Find First DataBank Unit, can't do DIC Lookup because of exact match check
 ;Returns Null or First DataBank Unit for text passed in
 ;Not a true conversion, because if Unit contains "/", we convert only piece 1
 N PSSVUTX,PSSVUTZ,PSSVUTAA,PSSVUTFL
 S PSSVUTFL=0 I $G(PSSVUTUN)="" S PSSVUTZ="" G UNITX
 S PSSVUTAA=$$UP^XLFSTR(PSSVUTUN)
UNITP ;
 K PSSVUTZ S PSSVUTX=$O(^PS(51.24,"B",PSSVUTAA,0)) I PSSVUTX,'$$SCREEN^XTID(51.24,.01,PSSVUTX_",") S PSSVUTZ=$P($G(^PS(51.24,PSSVUTX,0)),"^",2) I PSSVUTZ'="" G UNITX
 K PSSVUTZ S PSSVUTX=$O(^PS(51.24,"C",PSSVUTAA,0)) I PSSVUTX,'$$SCREEN^XTID(51.24,.01,PSSVUTX_",") S PSSVUTZ=$P($G(^PS(51.24,PSSVUTX,0)),"^",2) I PSSVUTZ'="" G UNITX
 K PSSVUTZ S PSSVUTX=$O(^PS(51.24,"D",PSSVUTAA,0)) I PSSVUTX,'$$SCREEN^XTID(51.24,.01,PSSVUTX_",") S PSSVUTZ=$P($G(^PS(51.24,PSSVUTX,0)),"^",2) I PSSVUTZ'="" G UNITX
 I 'PSSVUTFL,$G(PSSVUTUN)["/" S PSSVUTFL=1,PSSVUTAA=$P(PSSVUTUN,"/"),PSSVUTAA=$$UP^XLFSTR(PSSVUTAA) G:PSSVUTAA'="" UNITP
UNITX ;
 Q $G(PSSVUTZ)
 ;
 ;
FRQ(PSSFWSCC,PSSFWFR,PSSFWPK,PSSFWDRL) ;Return Daily Frequency for Daily Dose Check
 ;
 ;Input variables:
 ;PSSFWSC = Free Text Schedule
 ;PSSFWFR = Frequency in Minutes
 ;PSSFWPK = Package  "O" for Outpatient, "I" for Inpatient
 ;PSSFWDRL = Duration
 ;Output:
 ;Input for Frequency Field in API (can be numeric or text), or null
 ;
 ;
 N PSSFWPR1,PSSFWPR2,PSSFWPR3,PSSFWPR4,PSSFWPR5,PSSFWPR6,PSSFWPR7
 S PSSFWPR1=0
 S PSSFWPR2=$$FRQZ
 I PSSFWPR1 Q PSSFWPR2_"^"_$G(PSSFWPR7)
 S PSSFWPR3=$L(PSSFWSCC) I PSSFWPR3<5 Q PSSFWPR2_"^"_$G(PSSFWPR7)
 S PSSFWPR4=$E(PSSFWSCC,(PSSFWPR3-3),PSSFWPR3) S PSSFWPR4=$$UP^XLFSTR(PSSFWPR4)
 I PSSFWPR4'=" PRN" Q PSSFWPR2_"^"_$G(PSSFWPR7)
 S PSSFWPR5=PSSFWSCC
 S PSSFWSCC=$E(PSSFWSCC,1,(PSSFWPR3-4)) K PSSFWPR7
 S PSSFWPR6=$$FRQZ
 S PSSFWSCC=PSSFWPR5
 Q PSSFWPR6_"^"_$G(PSSFWPR7)
 ;
 ;
FRQZ() ;
 N PSSFWRST,PSSFWFLG,PSSFWSC
 S PSSFWSC=$$UP^XLFSTR(PSSFWSCC)
 K PSSFWRST
 I $G(PSSFWPK)'="O",$G(PSSFWPK)'="I" Q ""
 I $G(PSSFWFR)="D" D DAY Q $G(PSSFWRST)
 I $G(PSSFWFR) D NUMB I PSSFWFLG Q PSSFWRST
 D STN I PSSFWFLG Q PSSFWRST
 I $G(PSSFWPK)="O" D STNO I PSSFWFLG Q PSSFWRST
 Q ""
 ;
 ;
DAY ;Day of week schedule
 N PSSFWFND,PSSFWRGH,PSSFWLTH,PSSFWTMP,PSSFWLP,PSSFWLP1,PSSFWCNT,PSSFWQZ,PSSFWDIV,PSSFWNUM,PSSFWKZ1,PSSFWKZ2,PSSFWKZ3,PSSFWKZ4,PSSFWKZ5
 K PSSFWRST
 S PSSFWLTH=$L(PSSFWSC)
 S PSSFWFND=$F(PSSFWSC,"@")
 S PSSFWRGH=$E(PSSFWSC,PSSFWFND,PSSFWLTH)
 S PSSFWTMP=$S($E(PSSFWRGH,$L(PSSFWRGH))'="-":PSSFWRGH_"-",1:PSSFWRGH)
 I PSSFWTMP?.(2N1"-")!(PSSFWTMP?.(4N1"-")) D  S PSSFWRST=PSSFWCNT,PSSFWPR1=1 Q
 .S PSSFWCNT=0 F PSSFWLP=1:1:$L(PSSFWTMP) I $E(PSSFWTMP,PSSFWLP)="-" S PSSFWCNT=PSSFWCNT+1
 I PSSFWRGH'="" F PSSFWLP1=0:0 S PSSFWLP1=$O(^PS(51.1,"APPSJ",PSSFWRGH,PSSFWLP1)) Q:'PSSFWLP1!($G(PSSFWRST))  D
 .I $P($G(^PS(51.1,PSSFWLP1,0)),"^",5)'="D" Q
 .S PSSFWKZ1=$P($G(^PS(51.1,PSSFWLP1,0)),"^",2)
 .S PSSFWKZ5=0 I PSSFWKZ1'="" D
 ..S PSSFWKZ2=$S($E(PSSFWKZ1,$L(PSSFWKZ1))'="-":PSSFWKZ1_"-",1:PSSFWKZ1)
 ..I PSSFWKZ2?.(2N1"-")!(PSSFWKZ2?.(4N1"-")) D
 ...S PSSFWKZ3=0 F PSSFWKZ4=1:1:$L(PSSFWKZ2) I $E(PSSFWKZ2,PSSFWKZ4)="-" S PSSFWKZ5=PSSFWKZ5+1
 .I $G(PSSFWKZ5) S PSSFWRST=PSSFWKZ5
 I $G(PSSFWRST) S PSSFWPR1=1 Q
 I PSSFWRGH'="" F PSSFWLP1=0:0 S PSSFWLP1=$O(^PS(51.1,"APPSJ",PSSFWRGH,PSSFWLP1)) Q:'PSSFWLP1!($G(PSSFWRST))  D
 .K PSSFWQZ,PSSFWDIV
 .S PSSFWQZ=$P($G(^PS(51.1,PSSFWLP1,0)),"^",3)
 .Q:'$G(PSSFWQZ)
 .S PSSFWDIV=1440/PSSFWQZ I PSSFWDIV'>1 S PSSFWRST=1 Q
 .I PSSFWDIV?.N S PSSFWRST=PSSFWDIV,PSSFWPR1=1
 I $G(PSSFWRST) Q
 I PSSFWPK="O" D DAYOUT Q:$G(PSSFWRST)
 I PSSFWRGH?1"Q"1N.N1"H" S PSSFWRST=PSSFWRGH,PSSFWPR1=1 Q
 I $G(PSSFWSC)'["@" S PSSFWRST=1 Q
 I $E(PSSFWSC,$L(PSSFWSC))="@" S PSSFWRST=1 Q
 ;Else quit null
 Q
 ;
 ;
DAYOUT ;Day of week for Outpatient orders
 ;Ignore admin times in 51,different format
 ;START +8 PSSMIRPT for looping issue in File 51
 N PSSFWKZ6,PSSFWKZ7,PSSFWKZ8
 I PSSFWRGH'="" F PSSFWKZ6=0:0 S PSSFWKZ6=$O(^PS(51,"B",PSSFWRGH,PSSFWKZ6)) Q:'PSSFWKZ6!($G(PSSFWRST))  I '$G(^PS(51,"B",PSSFWRGH,PSSFWKZ6)) D
 .K PSSFWKZ7,PSSFWKZ8
 .S PSSFWKZ7=$P($G(^PS(51,PSSFWKZ6,0)),"^",8)
 .Q:'$G(PSSFWKZ7)
 .S PSSFWKZ8=1440/PSSFWKZ7 I PSSFWKZ8'>1 S PSSFWRST=1 Q
 .I PSSFWKZ8?.N S PSSFWRST=PSSFWKZ8,PSSFWPR1=1
 Q
 ;
 ;
NUMB ;Frequency passed in as a number
 S PSSFWFLG=0 K PSSFWRST
 N PSSFWDIS,PSSFWGRT,PSSFWMNT,PSSFWEEK,PSSFWXWK,PSSFWXMN
 S PSSFWDIS=1440/PSSFWFR I PSSFWDIS?.N S PSSFWFLG=1,PSSFWRST=PSSFWDIS,PSSFWPR1=1 Q
 I PSSFWDIS'<1 Q
 S PSSFWGRT=PSSFWFR/1440
 I PSSFWGRT?.N D  Q
 .S PSSFWMNT=PSSFWGRT/30 I PSSFWMNT?.N S PSSFWFLG=1,PSSFWRST="Q"_PSSFWMNT_"L",PSSFWPR1=1 Q
 .S PSSFWEEK=PSSFWGRT/7 I PSSFWEEK?.N S PSSFWFLG=1,PSSFWRST="Q"_PSSFWEEK_"W",PSSFWPR1=1 Q
 .S PSSFWFLG=1,PSSFWRST="Q"_PSSFWGRT_"D",PSSFWPR1=1 Q
 I PSSFWFR'>10080 S PSSFWXWK=10080/PSSFWFR I PSSFWXWK?.N S PSSFWFLG=1,PSSFWRST="X"_PSSFWXWK_"W",PSSFWPR1=1 Q
 S PSSFWXMN=43200/PSSFWFR I PSSFWXMN?.N S PSSFWFLG=1,PSSFWRST="X"_PSSFWXMN_"L",PSSFWPR1=1 Q
 Q
 ;
 ;
STN ;Standard Logic
 S PSSFWFLG=0 K PSSFWRST I $G(PSSFWSC)="" Q
 N PSSFWLP2,PSSFWAA,PSSFWAAD,PSSFWAAM,PSSFWAMN,PSSFWAWK,PSSFWAXL,PSSFWAXW
 F PSSFWLP2=0:0 S PSSFWLP2=$O(^PS(51.1,"APPSJ",PSSFWSC,PSSFWLP2)) Q:'PSSFWLP2!(PSSFWFLG)  D
 .K PSSFWAA,PSSFWAAD
 .S PSSFWAA=$P($G(^PS(51.1,PSSFWLP2,0)),"^",3)
 .Q:'$G(PSSFWAA)
 .S PSSFWAAD=1440/PSSFWAA
 .I PSSFWAAD?.N S PSSFWRST=PSSFWAAD,PSSFWFLG=1 Q
 .I PSSFWAAD>1 Q
 .S PSSFWAAM=PSSFWAA/1440
 .I PSSFWAAM?.N D  Q
 ..S PSSFWAMN=PSSFWAAM/30 I PSSFWAMN?.N S PSSFWFLG=1,PSSFWRST="Q"_PSSFWAMN_"L" Q
 ..S PSSFWAWK=PSSFWAAM/7 I PSSFWAWK?.N S PSSFWFLG=1,PSSFWRST="Q"_PSSFWAWK_"W" Q
 ..S PSSFWFLG=1,PSSFWRST="Q"_PSSFWAAM_"D" Q
 .I PSSFWAA'>10080 S PSSFWAXW=10080/PSSFWAA I PSSFWAXW?.N S PSSFWFLG=1,PSSFWRST="X"_PSSFWAXW_"W" Q
 .S PSSFWAXL=43200/PSSFWAA I PSSFWAXL?.N S PSSFWFLG=1,PSSFWRST="X"_PSSFWAXL_"L" Q
 I PSSFWFLG D DURLS I PSSFWFLG S PSSFWPR1=1 Q
 I PSSFWSC?1"Q"1N.N1"H" S PSSFWRST=PSSFWSC,PSSFWFLG=1 D DURLS I PSSFWFLG S PSSFWPR1=1
 Q
 ;
 ;
STNO ;Standard Logic part 2, using File 51, For Outpatient Orders only
 S PSSFWFLG=0 K PSSFWRST I $G(PSSFWSC)="" Q
 N PSSFWLP3,PSSFWBA,PSSFWBAD,PSSFWBAM,PSSFWBMN,PSSFWBWK,PSSFWBXL,PSSFWBXW
 F PSSFWLP3=0:0 S PSSFWLP3=$O(^PS(51,"B",PSSFWSC,PSSFWLP3)) Q:'PSSFWLP3!(PSSFWFLG)  I '$G(^PS(51,"B",PSSFWSC,PSSFWLP3)) D
 .K PSSFWBA,PSSFWBAD
 .S PSSFWBA=$P($G(^PS(51,PSSFWLP3,0)),"^",8)
 .Q:'$G(PSSFWBA)
 .S PSSFWBAD=1440/PSSFWBA
 .I PSSFWBAD?.N S PSSFWRST=PSSFWBAD,PSSFWFLG=1 Q
 .I PSSFWBAD>1 Q
 .S PSSFWBAM=PSSFWBA/1440
 .I PSSFWBAM?.N D  Q
 ..S PSSFWBMN=PSSFWBAM/30 I PSSFWBMN?.N S PSSFWFLG=1,PSSFWRST="Q"_PSSFWBMN_"L" Q
 ..S PSSFWBWK=PSSFWBAM/7 I PSSFWBWK?.N S PSSFWFLG=1,PSSFWRST="Q"_PSSFWBWK_"W" Q
 ..S PSSFWFLG=1,PSSFWRST="Q"_PSSFWBAM_"D" Q
 .I PSSFWBA'>10080 S PSSFWBXW=10080/PSSFWBA I PSSFWBXW?.N S PSSFWFLG=1,PSSFWRST="X"_PSSFWBXW_"W" Q
 .S PSSFWBXL=43200/PSSFWBA I PSSFWBXL?.N S PSSFWFLG=1,PSSFWRST="X"_PSSFWBXL_"L" Q
 I PSSFWFLG D DURLS I PSSFWFLG S PSSFWPR1=1 Q
 I PSSFWSC?1"Q"1N.N1"H" S PSSFWRST=PSSFWSC,PSSFWFLG=1 D DURLS I PSSFWFLG S PSSFWPR1=1
 Q
 ;
 ;
DS() ; Return 1 if Dose Checks are enabled, return 0 if Dose Checks are not enabled
 Q 0
 ;
 ;
IV(PSSADFOI) ;Return Additive Frequency default to CPRS, Forum DBIA 5425
 ;PSSADFOI = File 50.7 Internal Entry Number
 N PSSADFRS,PSSADFIN,PSSADFLP,PSSADFXX,PSSADFHD,PSSADFNN,PSSADFER,PSSADFCT
 S PSSADFRS="",(PSSADFXX,PSSADFCT)=0
 I '$G(PSSADFOI) Q PSSADFRS
 F PSSADFLP=0:0 S PSSADFLP=$O(^PS(52.6,"AOI",PSSADFOI,PSSADFLP)) Q:'PSSADFLP!(PSSADFXX)  D
 .S PSSADFIN=$P($G(^PS(52.6,PSSADFLP,"I")),"^")
 .I PSSADFIN,PSSADFIN'>DT Q
 .S PSSADFNN=PSSADFLP_","
 .S PSSADFHD=$$GET1^DIQ(52.6,PSSADFNN,18,"I",,"PSSADFER") I PSSADFHD="" S PSSADFXX=1 Q
 .I 'PSSADFCT S PSSADFRS=PSSADFHD S PSSADFCT=1 Q
 .I PSSADFHD'=PSSADFRS S PSSADFXX=1
 I PSSADFXX S PSSADFRS=""
 Q PSSADFRS
 ;
 ;
BSA(PSSBSADF) ;
 I '$G(PSSBSADF) Q "0^0"
 N DFN,VADM,VAPTYP,VAHOW,VAROOT,VAERR,VA,X1,X2,X,%Y,PSSBSAW1,PSSBSAW2,PSSBSAH1,PSSBSAH2,GMRVSTR,PSSBSAB2,PSSBSAH3
 S DFN=PSSBSADF
 S (PSSBSAW2,PSSBSAH2,PSSBSAB2,PSSBSAH3)=0
 S GMRVSTR="WT" K X D EN6^GMRVUTL
 S PSSBSAW1=$P(X,"^",8) I PSSBSAW1 S PSSBSAW2=PSSBSAW1/2.2
 S DFN=PSSBSADF
 S GMRVSTR="HT" K X D EN6^GMRVUTL
 S PSSBSAH1=$P(X,"^",8) I PSSBSAH1 S PSSBSAH2=.0254*PSSBSAH1,PSSBSAH3=$J(PSSBSAH1*2.54,6,2)
 ;Using DuBios formula for BSA calculation, and sending in 2 decimal places
 I $G(PSSBSAW2),$G(PSSBSAH2) S PSSBSAB2=.20247*(PSSBSAH2**.725)*(PSSBSAW2**.425)
 ;I $G(PSSBSAW2),$G(PSSBSAH2) S PSSBSAB2=$J((((PSSBSAW2*PSSBSAH2)/3600)**.5),0,2)    Mosteller BSA Formula
 Q PSSBSAH3_"^"_PSSBSAW2_"^"_PSSBSAB2
 ;
 ;
UNITD(PSSVUTUN) ;Find First DataBank Unit, can't do DIC Lookup because of exact match check
 ;Returns Null or First DataBank Unit for text passed in
 ;Not a true conversion, because if Unit contains "/", we convert only piece 1
 ;The difference between UNIT and UNITD tags is UNITD only returns data if DOse Form Infocator is Set to No
 N PSSVUTX,PSSVUTZ,PSSVUTAA,PSSVUTFL
 S PSSVUTFL=0 I $G(PSSVUTUN)="" S PSSVUTZ="" G UNITDX
 S PSSVUTAA=$$UP^XLFSTR(PSSVUTUN)
UNITDP ;
 K PSSVUTZ S PSSVUTX=$O(^PS(51.24,"B",PSSVUTAA,0)) I PSSVUTX,'$$SCREEN^XTID(51.24,.01,PSSVUTX_",") S PSSVUTZ=$P($G(^PS(51.24,PSSVUTX,0)),"^",2) I PSSVUTZ'="",$P($G(^PS(51.24,PSSVUTX,0)),"^",3)=0 G UNITDX
 K PSSVUTZ S PSSVUTX=$O(^PS(51.24,"C",PSSVUTAA,0)) I PSSVUTX,'$$SCREEN^XTID(51.24,.01,PSSVUTX_",") S PSSVUTZ=$P($G(^PS(51.24,PSSVUTX,0)),"^",2) I PSSVUTZ'="",$P($G(^PS(51.24,PSSVUTX,0)),"^",3)=0 G UNITDX
 K PSSVUTZ S PSSVUTX=$O(^PS(51.24,"D",PSSVUTAA,0)) I PSSVUTX,'$$SCREEN^XTID(51.24,.01,PSSVUTX_",") S PSSVUTZ=$P($G(^PS(51.24,PSSVUTX,0)),"^",2) I PSSVUTZ'="",$P($G(^PS(51.24,PSSVUTX,0)),"^",3)=0 G UNITDX
 K PSSVUTZ I 'PSSVUTFL,$G(PSSVUTUN)["/" S PSSVUTFL=1,PSSVUTAA=$P(PSSVUTUN,"/"),PSSVUTAA=$$UP^XLFSTR(PSSVUTAA) G:PSSVUTAA'="" UNITDP
UNITDX ;
 Q $G(PSSVUTZ)
 ;
 ;
DURLS ;If Duration is less that 24 hours, make Frequency adjustments if applicable
 ;Only check Frequencies of a whole number or in the format of Q#H
 N PSSDK1,PSSDK2,PSSDK3,PSSDK4,PSSDK5,PSSDK6
 S (PSSDK4,PSSFWPR7)=PSSFWRST
 ;Re-adjust frequency if needed, leave PSSFWFLG flag set to 1
 ;If Frequency needs to be Killed, also reset PSSFWFLG Flag back to 0
 I $G(PSSFWDRL)="" Q
 S PSSDK1=$$DRT^PSSDSAPD(PSSFWDRL) I PSSDK1'<1440!(PSSDK1'>0) Q
 S PSSDK2=1440/PSSDK1
 I PSSDK4?.N D  Q
 .S PSSDK5=PSSDK4/PSSDK2
 .I PSSDK5<1 K PSSFWRST S PSSFWFLG=0 Q
 .S PSSDK6=$J(PSSDK5,0,0)
 .S PSSFWRST=PSSDK6
 I PSSDK4?1"Q"1N.N1"H" D  Q
 .S PSSDK3=$$FRCON^PSSDSAPK(PSSDK4)
 .S PSSDK5=PSSDK3/PSSDK2
 .I PSSDK5<1 K PSSFWRST S PSSFWFLG=0 Q
 .S PSSDK6=$J(PSSDK5,0,0)
 .S PSSFWRST=PSSDK6
 Q
