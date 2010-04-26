(* ::Package:: *)

BeginPackage["TRP`"]

Symbol_Key;
Symbol_Both;

TRPArrayPlot::usage = 
"TRPArrayPlot[ matrix, <options>]
	
  KeyUnits \[Rule] \"\"
  ImageHeight \[Rule] 300
  ColorFunction \[Rule] GrayLevel
  DataMax \[Rule] Max[matrix]
  DataType -> \"Attenuation\" | \"ReadRate\"
  Raw \[Rule] False | Key | Plot";

ImportTRPData::usage =
"usage information goes here";

TRPTicks::usage =
"TRPTicks[ {<max plot coordinates>}, {<real world max dimensions>}, <options>]
TRPTicks[ {<max plot coordinates>}, {<real world max dimensions>}, interval, <options>]

	Units->\"cm\"";

GradientCuboid::usage = 
"GradientCuboid[ start, end, <options> ]
  
   Mesh -> 150,
   ColorFunction -> GrayLevel";

Symmetry::usage = 
"Symmetry[ matrix ]";


Begin["`Private`"]

TRPImageHeight = 300;


ImportTRPData[file_String] := Import[file, "Table", FieldSeparators -> {",", "(", ")"}];


TRPDataDimensions[data_] :=
  If[#==Round[#],Round[#],#]&/@Abs[Min[data[[All, #]]] - Max[data[[All, #]]] & /@ {7, 5}]/10;


TRPKey[max_, OptionsPattern[{KeyUnits -> "", ImageHeight -> TRPImageHeight, ColorFunction -> GrayLevel}]] :=
  Module[{n},
  n = N[max];
  n = If[n == Round[n], Round[n], n];  
  DensityPlot[y, {x, 0, 1}, {y, 0, 10}, 
    ColorFunction -> OptionValue[ColorFunction], 
    FrameLabel -> {{None, None}, {0, 
       ToString[n] <> " " <> 
        OptionValue[KeyUnits]}}, 
    ImageSize -> {Automatic, OptionValue[ImageHeight]}, Frame -> True,
     FrameTicks -> False, PlotRangePadding -> 0, 
    PlotRangeClipping -> False, PlotRegion -> {{0, 1}, {0, 1}}, 
    AspectRatio -> 10]]


TRPKeyFromData[mat_, opts : OptionsPattern[{DataMax -> 0, KeyUnits -> "", ImageHeight -> TRPImageHeight, ColorFunction -> GrayLevel}]] :=
	TRPKey[OptionValue[DataMax],FilterRules[{opts},Except[DataMax]]];


TRPArrayPlot[mat_, 
  opts : OptionsPattern[
    Join[{ColorFunction -> GrayLevel, DataMax -> Max, Axes -> True, Mesh -> True, DataType -> "Attenuation", KeyUnits -> "", 
      Raw -> False, ImageHeight -> TRPImageHeight},Options[ArrayPlot]]]] := 
 Module[{key, ap, cf}, 
If[MatchQ[OptionValue[ColorFunction],_Function],cf=OptionValue[ColorFunction],
  If[OptionValue[DataType]=="Attenuation", cf=GrayLevel,
	If[OptionValue[DataType]=="ReadRate", cf=(GrayLevel[1-#]&)]]];
  If[Count[{OptionValue[DataMax]}, _Symbol | _Function] > 0, maxValue = OptionValue[DataMax]@mat, maxValue = OptionValue[DataMax]];
  key = TRPKeyFromData[mat, DataMax->maxValue,ColorFunction->cf, KeyUnits -> OptionValue[KeyUnits], ImageHeight -> OptionValue[ImageHeight]];
  ap = ArrayPlot[mat, 
    Join[{PlotRange -> {All, All, maxValue}, 
      ColorFunction -> cf, 
      ImageSize -> {Automatic, OptionValue[ImageHeight]}, Mesh->OptionValue[Mesh], Axes->OptionValue[Axes],Ticks -> OptionValue[Ticks], AxesOrigin -> {-1, -1}, 
      Frame -> False},FilterRules[{opts}, Options[ArrayPlot]]]];
  	Return[
If[OptionValue[Raw]===False, 
	Row[{ap, key}, Spacer[10],ImageSize -> {Automatic, OptionValue[ImageHeight]}],
	If[OptionValue[Raw] === Key, key, If[OptionValue[Raw] === Plot, ap,If[OptionValue[Raw]===Both,{ap, key}]]] 	   
		]]];

GradientCuboid[point1_ :{0,0,0},point2_ : {1,1,1},OptionsPattern[{Mesh->50,ColorFunction->GrayLevel}]]:=(top=(point2-point1)[[3]];
m=OptionValue[Mesh];
Graphics3D[Flatten[{
FaceForm[OptionValue[ColorFunction][#/top]],
EdgeForm[],
Cuboid[point1 +{0,0,#},
Join[point2[[{1,2}]],{#+top/m}]]}&/@(Range[0,m-1] top/m)],Lighting->"Neutral",Boxed->False]);


OldTRPTicks[maxes_List, dims_List,steps_] := 
Function[{m, d}, Join[{##, Round[## steps/10]} & /@ Range[0, m, 10], {{m, ToString[d] <> "cm"}}]] @@@ Transpose[Join[{maxes}, {dims}]];

TRPTicks[maxes_List, dims_List, intervals_ : 10, OptionsPattern[{Units -> "cm"}]] := 
 Module[{steps, ticks, newDims}, steps = maxes/dims;
  ticks = 
   Join[{Range[0, maxes[[1]], intervals steps[[1]]]}, {Range[0, maxes[[2]], 
      intervals steps[[2]]]}];
  newDims = Range[0, #, intervals ] & /@ dims;
  If[Max[ticks[[1]]] < maxes[[1]],
   ticks[[1]] = Append[ticks[[1]], maxes[[1]]];
   newDims[[1]] = Append[newDims[[1]], dims[[1]]];];
  If[Max[ticks[[2]]] < maxes[[2]], 
   ticks[[2]] = Append[ticks[[2]], maxes[[2]]];
   newDims[[2]] = Append[newDims[[2]], dims[[2]]];];
  newDims = 
   Append[Drop[##, -1], ToString[Last[##]] <> OptionValue[Units]] & /@ newDims; 
  Return[Transpose /@ Transpose[{ticks, newDims}]]]






Symmetry[mat_List] := Module[{height,width,center,oddOffset,offset},{height,width}=Dimensions[mat];
	halfwidth=IntegerPart[width/2];
	Manipulate[{ Show[
		TRPArrayPlot[mat,Mesh->False,Raw->Plot],
	  Graphics[{Red,Line[{{center,0},{center,height}}],
		If[odd,Line[{{center+1,0},{center+1,height}}],{}]}]],
		oddOffset=If[odd,1,0];
		offset=If[center<width/2,center,width-center-oddOffset];
		N[Correlation[Flatten[mat[[All,Range[center-offset+1,center]]]],
Flatten[Reverse[mat[[All,Range[center+1+oddOffset,center+offset+oddOffset]]],2]]]]},
		{{center,IntegerPart[width/2],"Center"},1,width-1-If[odd,1,0],1},{{odd,False,"Odd"},{False,True}}]]
End[];
EndPackage[];






