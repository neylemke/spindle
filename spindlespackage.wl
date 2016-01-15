(* ::Package:: *)

lischannels={"f3","f4","c3","c4","p3","p4","o1","o2"} ;
listimes={"TimemaxPropf3","TimemaxPropf4","TimemaxPropc3","TimemaxPropc4","TimemaxPropp3","TimemaxPropp4","TimemaxPropo1","TimemaxPropo2"};
lisEnv={"EnvmaxPropf3","EnvmaxPropf4","EnvmaxPropc3","EnvmaxPropc4","EnvmaxPropp3","EnvmaxPropp4","EnvmaxPropo1","EnvmaxPropo2"};
delays={"TimePropf3f4","TimePropf3c3","TimePropf3c4","TimePropf3p3","TimePropf3p4","TimePropf3o1","TimePropf3o2","TimePropf4c3","TimePropf4c4","TimePropf4p3","TimePropf4p4","TimePropf4o1","TimePropf4o2","TimePropc3c4","TimePropc3p3","TimePropc3p4","TimePropc3o1","TimePropc3o2","TimePropc4p3","TimePropc4p4","TimePropc4o1","TimePropc4o2","TimePropp3p4","TimePropp3o1","TimePropp3o2","TimePropp4o1","TimePropp4o2","TimePropo1o2"};
opts={TableHeadings->{Style[#,FontFamily->"Times",18]&/@lischannels,Style[#,FontFamily->"Times",18]&/@lischannels},TableAlignments->Center};
lisdeltas="DeltafracFreq"<>#&/@lischannels;
lisenv="EnvmaxProp"<>#&/@lischannels;
lisFreq="FreqSS"<>#&/@lischannels;
lisChirp="ChirpSS"<>#&/@lischannels;
lisDura="IduracProp"<>#&/@lischannels;
hashVarNames[x_]=x;
hashVarNames["TKOPglob"]="T";
hashVarNames["rKOPglob"]="r";
hashVarNames["ChirpSSglob"]="Chirp";
hashVarNames["mKOPglob"]="m";
hashVarNames["vel"]="Velocity";
hashVarNames["FreqSSglob"]="Frequency";
hashVarNames["IduracPropglob"]="Duration";


(* ::Input:: *)
(*geradelay[{i_,j_},v_,{x_,y_,z_}]:=(Norm[lischannelspos[[i]]-{x,y,z}]-Norm[lischannelspos[[j]]-{x,y,z}])/v;*)
(*geraOrigemOld[lischannelspos_,delays_,{{xmin_,xmax_},{ymin_,ymax_},{zmin_,zmax_}},step_,{vmin_,vmax_},stepv_]:=Module[{eq,v},*)
(*First[Sort[Flatten[Table[{{{x,y,z},v},Sum[((Norm[(lischannelspos[[i]]-{x,y,z})]-Norm[(lischannelspos[[j]]-{x,y,z})])/v-delays[[i,j]])^2,{j,2,8},{i,1,j-1}]},{x,xmin,xmax,step},{y,ymin,ymax,step},{z,zmin,zmax,step},{v,vmin,vmax,stepv}],3],#2[[2]]>#1[[2]]&]]]*)


(* ::Input:: *)
(*geradelay[{i_,j_},v_,{x_,y_,z_}]:=(Norm[lischannelspos[[i]]-{x,y,z}]-Norm[lischannelspos[[j]]-{x,y,z}])/v;*)
(*geraOrigem[lischannelspos_,delays_,{{xmin_,xmax_},{ymin_,ymax_},{zmin_,zmax_}},step_,{vmin_,vmax_},stepv_]:=Module[{eq,v},*)
(*eq=Sum[((Norm[(lischannelspos[[i]]-{x,y,z})]-Norm[(lischannelspos[[j]]-{x,y,z})])/v-delays[[i,j]])^2,{j,2,8},{i,1,j-1}];*)
(*First[Sort[Flatten[Table[{{{x,y,z},v},eq},{x,xmin,xmax,step},{y,ymin,ymax,step},{z,zmin,zmax,step},{v,vmin,vmax,stepv}],3],#2[[2]]>#1[[2]]&]]]*)
(**)
(*Dip[lis_]:=Module[{lisname,val,filename,Rcommand},*)
(*filename="listemp2.tsv";*)
(*Export["/Users/rafaeltoledo/Documents/MATLAB/Codigos EEG/ExamesTotal/ok1/math/"<>filename,StringReplace[StringDrop[StringDrop[ToString[lis],1],-1],","->" "],"TSV"];Rcommand="*)
(*library(diptest)*)
(*dipval<-dip(scan(\"/Users/rafaeltoledo/Documents/MATLAB/Codigos EEG/ExamesTotal/ok1/"<>filename<>"\"))*)
(*dipval*)
(*q()*)
(*";*)
(*Export["/Users/rafaeltoledo/Documents/MATLAB/Codigos EEG/ExamesTotal/ok1/dip2.R",Rcommand,"String"];*)
(*Run["/Library/Frameworks/R.framework/Resources/R --file=/Users/rafaeltoledo/Documents/MATLAB/Codigos EEG/ExamesTotal/ok1/dip2.R | tail -2 | head -1 | awk '{print $2}'> /Users/rafaeltoledo/Documents/MATLAB/Codigos EEG/ExamesTotal/ok1/kkkk"];*)
(*val=ToExpression[Import["/Users/neylemke/pesquisa/hipnogramas/kkkk"]];*)
(*Run["rm /Users/rafaeltoledo/Documents/MATLAB/Codigos EEG/ExamesTotal/ok1/kkkk"];*)
(*If[Length[lis]<5,0,val]*)
(*]*)
(**)
(*lischannelspos={{0.400,-0.450,0.360},{-0.394,-0.455,0.365},{0.556,0.180,0.430},{-0.543,0.180,0.433},{0.406,0.730,0.330},{-0.390,0.730,0.330},{0.269,1.00,-0.2},{-0.256,1.00,-0.200}};*)


(* ::Input:: *)
(*geraCorrTabTex[corrTab_,lisvar_]:=Module[{headerString,colString},headerString=StringJoin[ " & "<> #&/@ hashVarNames/@lisvar]<>"\\\\\n \\midrule\n ";*)
(*colString="l|"<>StringJoin[Table["c",{i,1,Length[corrTab]}]];*)
(*"\n\n\\begin{tabular}{"<>colString<>"}\n\\toprule\n"<> headerString<>StringJoin[Table[StringJoin[" "<> hashVarNames[lisvar[[j]]], Table[" & \\nprounddigits{2}\\numprint{"<> ToString[CForm[corrTab[[i,j]]]]<>"}",{i,1,Length[corrTab]}]]<> "\\\\\n",{j,1,Length[corrTab]}]]<>" \\bottomrule\n\\end{tabular}"]*)
(**)
(*gerapvalueCorrTabTex[corrTab_,lisvar_]:=Module[{},"\n\n\\begin{tabular}{"<>colString<>"}\n\\toprule\n"<> headerString<>StringJoin[Table[StringJoin[" "<> hashVarNames[lisvar[[j]]], Table[" & \\nprounddigits{3}\\numprint{"<> ToString[CForm[If[corrTab[[i,j]]<0.00000001,0,corrTab[[i,j]]]]]<>"}",{i,1,Length[corrTab]}]]<> "\\\\\n",{j,1,Length[corrTab]}]]<>" \\bottomrule\n\\end{tabular}"*)
(*]*)
(*geraCorrSmoothChannels[lisvar_]:=Module[{},*)
(*TableForm[Table[SmoothDensityHistogram[Transpose[{Normal[dataset2[All,lisvar[[i]]]],Normal[dataset2[All,lisvar[[j]]]]}]],{i,1,8},{j,1,8}],TableHeadings->{Style[#,FontFamily->"Times",32]&/@lischannels,Style[#,FontFamily->"Times",32]&/@lischannels},TableAlignments->Center]]*)
(*geraDataset[csv_]:=Module[{header,data},*)
(*header=First[csv];*)
(*header=StringReplace[header,"_"->""];*)
(*data=Rest[csv];*)
(*Dataset[AssociationThread[header->#]&/@data]]*)
(**)
(*geralinhaiah[dataset_,datasetpacientes_,var_]:=Module[{lisiah,lis1,lis2,partpacientes},*)
(*partpacientes=Normal@GroupBy[dataset,"Examindexglob"];*)
(*Print[Mean[Dataset[partpacientes[#]][All,var]]&/@Keys[partpacientes]];*)
(*lisiah=Function[key,{Mean[Dataset[partpacientes[key]][All,var]],First@Normal@(Select[datasetpacientes,#Exame==key&])[All,"IAH"]}]/@Keys[partpacientes];*)
(*lis1=Select[lisiah,Last[#]>=5&];*)
(*lis2=Select[lisiah,Last[#]<5&];*)
(*{Mean@(First/@lis1),Mean@(First/@lis2),LocationTest[{First/@lis1,First/@lis2}]}]*)
