PXRRPCEQ ;HIN/MjK - Clinic Specfic Workload Reports ;6/7/96
 ;;1.0;PCE PATIENT CARE ENCOUNTER;;Aug 12, 1996
 ;_._._._._._._._._._._._.Kill PCE vairiable_._._._._._._._._._._._._
Q W ! K ^TMP($J)
 K ZTSK,ZTDESC,ZTRTN,ZTREQ,ZTSAVE,ZTIO,ZTDTH,ZTUCI,DGUTQND,DGVAR,VAR,DGPGM,PGM,DGZTSAVE,DGI,IO("Q"),IO("C")
 K %,%DT,%Y,BEGDATE,DFN,DGPGM,DGVAR,DIV,ENDDATE,I,I1,J,J1,K,K1,L,L1,M,M1,N,N1,P,P1,POP,Q,Q1,R,S,SD1,SDADD,SDAED,SDALL,SDAPT,SDAS,SDB,SDBD,SDBO,SDCA,SDCL,SDCR,SDCUR,SDD,SDDIV,SDE,SDED,SDEO,SDF,SDF1,SDFL,SDHK,SDHR
 K SDF2,SDI,SDIN,SDN,SDNAM,SDNM,SDNOW,SDNS,SDNUM,SDOB,SDOLD,SDP,SDPG,SDPN,SDPRE,SDRT,SDS,SDSC,SDSCC,SDSCH,SDSCI,SDSCN,SDSCO,SDSCS,SDSCU,SD,SSN,SDST,SDSTAT,SDSUB,SDSSN,SDT,SDTOT,SDUN,SDV,VAUTC,VAUTD,VAUTNI
 K PXRRCLIN,PXRRCNUM,PXRRBDT,PXRREDT,PXRRSXMO,PXRRLED,PXRRYR,PXRRSMYR,PXRRDIFF,PXRROPT,PXRRCASE,PXRRDEMG,PXRC,PXR,PXRSTPNM,PXRCLNUM
CLEAN K PXRRTOT,PXRRPCT,PXRRENTD,PXRRNENT,PXRSNEW,PXRSEST,PXRSCON,PXRRUN,PXRRNS,PXRRCA,PXRRCL,PXRRAV,PXRRVCPT,PXRRVDT,PXRRVIFN,PXRRTVCO,PXRRNCPT,PXRI,PXRRY
 K PXRLRDFN,PXRRQ,PXRRSSN,PXRRTVS,PXRRTPAT,PXRRHTN,PXRRDIAB,PXRRHTDB,PXRRCAD,PXRRDOB,PXRRDFN,PXRRLDCD,PXRRHBPT,PXRRLDPT,PXRR,PXRRX,A,B,C,E,F,G,H,T,V,W,X,Y,Z,PX
 K PXRRSXUN,PXRRSXHP,PXRRSXER,PXRRSESS,PXRRNOLD,PXRRMMYR,PXRRMAMG,PXRRLED,PXRRLDL,PXRRHTDM,PXRRHLIP,PXRRHBA1,PXRRDM,PXRRCDSM,PXRRAG,PXRRAGE,LRDFN,PXRRMPAT,PXRRFPAT,PXRRSC
 K PXRRD250,PXRRD401,PXRRPTSS,PXRRQPAT,PXRRRTVS,PXRRUTVS,PXRRVPAT,PXRRHBG7,PXRRGL,PXRRCHOL,PXRRCDSX,PXRRPSUT,PXRRK,PXRRT,PXRRVLIS,PXRJ,PXRRG,PXRRH,PXRRK,PXRRN,PXRRAPT,PSOACT,PXRRCOST,PXRRPSO,PXRRF50
 K PXRRBIEN,PXRRBPT,PXRRER,PXRRA D KVAR^VADPT Q
