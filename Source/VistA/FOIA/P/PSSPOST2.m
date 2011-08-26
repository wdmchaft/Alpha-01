PSSPOST2 ;BIR/LDT-Post install routine ;08/01/00
 ;;1.0;PHARMACY DATA MANAGEMENT;**38**;9/30/97
 ;External reference to ORY94 supported by DBIA 3415
 ; POST-INSTALL ROUTINE
 I $$PATCH^XPDUTL("PSS*1.0*38") G END ; has patch already been installed
RES ;
 I '$G(DT) S DT=$$DT^XLFDT
 ;Delete AD cross references on Orderable Items in Files 52.6 and 52.7
 D DELIX^DDMOD(52.7,9,2)
 D DELIX^DDMOD(52.6,15,2)
 ;Delete CHANGE TYPE OF ORDER FROM OERR field (#20.412) and IV IDENTIFIER field (#32) from the PHARMACY SYSTEM file (#59.7)
 D BMES^XPDUTL("Deleting obsolete fields...")
 S DA=20.412 S DIK="^DD(59.7,",DA(1)=59.7 D ^DIK K DIK
 ;Delete CORRESPONDING UD ITEM field (#3) and CORRESPONDING IV ITEM field (#4) from the PHARMACY ORDERABLE ITEM file (#50.7)
 F DA=3,4 S DIK="^DD(50.7,",DA(1)=50.7 D ^DIK K DIK
 ;Delete Instructions multiple and Pre-Ordering Enhancement field
 S DA=2,DA(1)=50.6066,DIK="^DD(50.6066," D ^DIK K DA,DIK
 S DA=4,DIK="^DD(50.606,",DA(1)=50.606 D ^DIK K DA,DIK
 K DIU S DIU=50.6062,DIU(0)="S" D EN^DIU2 K DIU
 D BMES^XPDUTL("Deleting obsolete data...")
 N PSSDF,PSSDF1 F PSSDF=0:0 S PSSDF=$O(^PS(50.606,PSSDF)) Q:'PSSDF  D
 .K ^PS(50.606,PSSDF,"INS")
 .I $D(^PS(50.606,PSSDF,"MISC")) S $P(^PS(50.606,PSSDF,"MISC"),"^",2)=""
 .F PSSDF1=0:0 S PSSDF1=$O(^PS(50.606,PSSDF,"NOUN",PSSDF1)) Q:'PSSDF1  I $D(^PS(50.606,PSSDF,"NOUN",PSSDF1,0)) S $P(^(0),"^",3)=""
 N PSSPSF S PSSPSF=0 F  S PSSPSF=$O(^PS(59.7,PSSPSF)) Q:'PSSPSF  S $P(^PS(59.7,PSSPSF,20.4),"^",16)=""
 N PSSOI S PSSOI=0 F  S PSSOI=$O(^PS(50.7,PSSOI)) Q:'PSSOI  S $P(^PS(50.7,PSSOI,0),"^",10)="",$P(^(0),"^",11)=""
 ;
 ;fix options (matching) DISABLE A/SMATCHING, MODIFY DD, FIX FROM DRIG ENTER.EDIT
 ;D QUICK^ORUPDATE ; loop through orders, call PSSQORD
 ;This quick order update may take a while, can CPRS write dots
CON N PSSL,PSSPOI,PSSADD,PSSSOL,PSSDRG,PSSNEWOI,PSSOLDOI,PSSXDATE
 ;Converting Additives and Solutions to new Orderable Items
 S X1=DT,X2=+30 D C^%DTC S PSSXDATE=$G(X)
 S ^XTMP("PSSCONA",0)=PSSXDATE_"^"_DT,^XTMP("PSSCONS",0)=PSSXDATE_"^"_DT
 D BMES^XPDUTL("Updating IV Additive Orderable Items...")
 F PSSL=0:0 S PSSL=$O(^PS(52.6,PSSL)) Q:'PSSL  D:$D(^PS(52.6,PSSL,0))
 .S PSSOLDOI=$P($G(^PS(52.6,PSSL,0)),"^",11) S $P(^PS(52.6,PSSL,0),"^",11)=""
 .I PSSOLDOI K ^PS(52.6,"AOI",PSSOLDOI,PSSL)
 .S PSSDRG=$P($G(^PS(52.6,PSSL,0)),"^",2) I 'PSSDRG Q
 .S PSSNEWOI=$P($G(^PSDRUG(PSSDRG,2)),"^") D  I 'PSSNEWOI Q
 ..I PSSOLDOI,PSSNEWOI S:'$D(^PSDRUG("A526",PSSDRG,PSSL)) ^XTMP("PSSCONA",PSSOLDOI,PSSL)=PSSNEWOI
 .I '$D(^PS(50.7,PSSNEWOI,0)) Q
 .S $P(^PS(52.6,PSSL,0),"^",11)=PSSNEWOI,^PS(52.6,"AOI",PSSNEWOI,PSSL)="",^PSDRUG("A526",PSSDRG,PSSL)=""
 D BMES^XPDUTL("Updating IV Solution Orderable Items...")
 F PSSL=0:0 S PSSL=$O(^PS(52.7,PSSL)) Q:'PSSL  D:$D(^PS(52.7,PSSL,0))
 .S PSSOLDOI=$P($G(^PS(52.7,PSSL,0)),"^",11) S $P(^PS(52.7,PSSL,0),"^",11)=""
 .I PSSOLDOI K ^PS(52.7,"AOI",PSSOLDOI,PSSL)
 .S PSSDRG=$P($G(^PS(52.7,PSSL,0)),"^",2) I 'PSSDRG Q
 .S PSSNEWOI=$P($G(^PSDRUG(PSSDRG,2)),"^") D  I 'PSSNEWOI Q
 ..I PSSOLDOI,PSSNEWOI S:'$D(^PSDRUG("A527",PSSDRG,PSSL)) ^XTMP("PSSCONS",PSSOLDOI,PSSL)=PSSNEWOI
 .I '$D(^PS(50.7,PSSNEWOI,0)) Q
 .S $P(^PS(52.7,PSSL,0),"^",11)=PSSNEWOI,^PS(52.7,"AOI",PSSNEWOI,PSSL)="",^PSDRUG("A527",PSSDRG,PSSL)=""
OI ;Updating CPRS with new Orderable Item information
 N PSSORITM,PSSRI,PSSORIDT,PSSCONTX,PSSRD1,PSSRD2
 D BMES^XPDUTL("Setting new Orderable Item cross reference...")
 S PSSCONTX=0
 F PSSRD1=0:0 S PSSRD1=$O(^PSDRUG(PSSRD1)) Q:'PSSRD1  D
 .S PSSRD2=$P($G(^PSDRUG(PSSRD1,2)),"^") Q:'PSSRD2
 .S ^PS(50.7,"A50",PSSRD2,PSSRD1)=""
 .I '(PSSCONTX#50) W "."
 .S PSSCONTX=PSSCONTX+1
 S PSSCONTX=0
 D BMES^XPDUTL("Updating Pharmacy Orderable Items...")
 I '$G(DT) S DT=$$DT^XLFDT
 F PSSRI=0:0 S PSSRI=$O(^PS(50.7,PSSRI)) Q:'PSSRI  D:$D(^PS(50.7,PSSRI,0))
 .S PSSORITM=PSSRI
 .S $P(^PS(50.7,PSSORITM,0),"^",12)=""
 .I $P(^PS(50.7,PSSORITM,0),"^",3) D
 ..S PSSORIDT=$P(^PS(50.7,PSSORITM,0),"^",4) I PSSORIDT,PSSORIDT'>DT Q
 ..S $P(^PS(50.7,PSSORITM,0),"^",4)=DT
 .S PSSCROSS=1,PSSTEST=PSSORITM D EN1^PSSPOIDT K PSSTEST,PSSCROSS
 .I '(PSSCONTX#10) W "."
 .S PSSCONTX=PSSCONTX+1
 ;D BMES^XPDUTL("Queue Inpatient Medications order conversion...")
 ;S ZTDTH=$H,ZTRTN="CNIV^PSJUTL1()",ZTIO="",ZTDESC="Inpatient medications order conversion" D ^%ZTLOAD
 ;D EN^PSOPOST2
 D POST^ORY94
 ;D POE^PSJPST50
MAIL ;
 D NOW^%DTC S PSSTIMEB=%
 S Y=$G(^XTMP("PSSTIMEX","START")) D DD^%DT S PSSTIMEA=Y
 S Y=$G(PSSTIMEB) D DD^%DT S PSSTIMEB=Y
 S XMDUZ="PHARMACY DATA MANAGEMENT PACKAGE",XMY(DUZ)="",XMSUB="Pharmacy Ordering Enhancements Install"
 K PSSTEXT S PSSTEXT(1)="The Pharmacy Ordering Enhancements installation is complete.",PSSTEXT(2)="It started on "_$G(PSSTIMEA)_".",PSSTEXT(3)="It ended on "_$G(PSSTIMEB)_"."
 S XMTEXT="PSSTEXT(" N DIFROM D ^XMD
 K PSSTIMEA,PSSTIMEB,XMDUZ,XMSUB,PSSTEXT,XMTEXT
END ;
 Q
RESTART ;restart post init job
 G RES
 Q
