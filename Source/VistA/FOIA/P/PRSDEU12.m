PRSDEU12 ;HISC/GWB-PAID EDIT AND UPDATE DOWNLOAD RECORD 12 LAYOUT ;6/1/93  15:32
 ;;4.0;PAID;;Sep 21, 1995
 F CC=1:1 S GRP=$T(@CC) Q:GRP=""  S GRPVAL=$P(RCD,":",CC) I GRPVAL'="" S GNUM=$P(GRP,";",4),LTH=$P(GRP,";",5),PIC=$P(GRP,";",6) D:PIC=9 PIC9^PRSDUTIL F EE=1:1:GNUM S FLD=$T(@CC+EE) D EPTSET^PRSDSET
 Q
RECORD ;;Record 12;7
 ;;
1 ;;Group 1;7;29;X
 ;;TPDEPT;TSP VA DEPARTMENT CODE;1;2;TSP1;17;S:DATA="00" DATA="";;;411
 ;;TPSTATUS;TSP STATUS;3;3;TSP1;15;;;;409
 ;;TPSTADTE;TSP STATUS DATE;4;11;TSP1;16;D DATE^PRSDUTIL;;;410
 ;;TPSCD;TSP SERVICE COMPU DATE;12;19;TSP1;12;D DATE^PRSDUTIL;;;406
 ;;TPVEST;TSP VESTING;20;20;TSP1;18;;;;412
 ;;TPSEPCDE;TSP SEPARATION CODE;21;21;TSP1;13;;;;407
 ;;TPSEPDTE;TSP SEPARATION DATE;22;29;TSP1;14;D DATE^PRSDUTIL;;;408
 ;;
2 ;;Group 2;8;26;9
 ;;TPERDED;TSP PCT RATE;1;3;TSP1;10;S L=3 D LZ^PRSDUTIL,SIGN^PRSDUTIL,DDD^PRSDUTIL;;;404
 ;;TPEADED;TSP AMT EPPD;4;8;TSP1;2;D SIGN^PRSDUTIL S DATA=+DATA;;;396
 ;;TPERDIST-G;TSP GSF DIST PCT;9;11;TSP2;12;D SIGN^PRSDUTIL,DD^PRSDUTIL;;;424
 ;;TPERDIST-F;TSP FIF DIST PCT;12;14;TSP2;7;D SIGN^PRSDUTIL,DD^PRSDUTIL;;;419
 ;;TPERDIST-C;TSP CSF DIST PCT;15;17;TSP2;2;D SIGN^PRSDUTIL,DD^PRSDUTIL;;;414
 ;;TPVABDIST-G;TSP GSF GOV CONTRIB DIST PCT;18;20;TSP2;14;D SIGN^PRSDUTIL,DD^PRSDUTIL;;;426
 ;;TPVABDIST-F;TSP FIF GOV CONTRIB DIST PCT;21;23;TSP2;9;D SIGN^PRSDUTIL,DD^PRSDUTIL;;;421
 ;;TPVABDIST-C;TSP CSF GOV CONTRIB DIST PCT;24;26;TSP2;4;D SIGN^PRSDUTIL,DD^PRSDUTIL;;;416
 ;;
3 ;;Group 3;6;41;X
 ;;TPELIGDTE;TSP ELIG;1;8;TSP1;5;D DATE^PRSDUTIL;;;399
 ;;TPCYDIST-PR-APT;TSP DED PRIOR APPT YTD;9;15;TSP1;4;D SIGN^PRSDUTIL,DD^PRSDUTIL;;;398
 ;;TPLOAN-ACCT-NO;TSP LOAN ACCOUNT NUMBER;16;32;TSP1;6;;;;400
 ;;TPLOAN-AMT;TSP LOAN DED EPPD;33;39;TSP1;8;D SIGN^PRSDUTIL,DD^PRSDUTIL;;;402
 ;;MZDMSAREA;VHA REGION NUMBER;40;40;1;41;;;;88
 ;;MZNCSDIST;VCS AREA;41;41;VCS;14;;;;579
 ;;
4 ;;Group 4;1;23;X
 ;;MXRESAD1;RESIDENCE ADDRESS LINE 1;1;23;ADD;7
 ;;
5 ;;Group 5;1;23;X
 ;;MXRESAD1;RESIDENCE ADDRESS LINE 2;1;23;ADD;8
 ;;
6 ;;Group 6;1;16;X
 ;;MXRESAD3;RESIDENCE ADDRESS LINE 3;1;16;ADD;9
 ;;
7 ;;Group 7;1;9;X
 ;;MXRESZIP;RESIDENCE ADDRESS ZIP CODE;1;9;ADD;10;D ZIP^PRSDUTIL
