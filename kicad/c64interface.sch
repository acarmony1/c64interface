EESchema Schematic File Version 4
EELAYER 30 0
EELAYER END
$Descr A4 11693 8268
encoding utf-8
Sheet 1 1
Title "C64 Userport Interface"
Date "2021-03-30"
Rev "V1"
Comp "Carmony Technology"
Comment1 ""
Comment2 ""
Comment3 ""
Comment4 ""
$EndDescr
Wire Wire Line
	2300 2000 2300 1800
Wire Wire Line
	2300 2200 2300 2400
$Comp
L power:+12V #PWR0101
U 1 1 606289D2
P 2300 1800
F 0 "#PWR0101" H 2300 1650 50  0001 C CNN
F 1 "+12V" H 2315 1973 50  0000 C CNN
F 2 "" H 2300 1800 50  0001 C CNN
F 3 "" H 2300 1800 50  0001 C CNN
	1    2300 1800
	1    0    0    -1  
$EndComp
$Comp
L power:GND #PWR0102
U 1 1 60629023
P 2300 2400
F 0 "#PWR0102" H 2300 2150 50  0001 C CNN
F 1 "GND" H 2305 2227 50  0000 C CNN
F 2 "" H 2300 2400 50  0001 C CNN
F 3 "" H 2300 2400 50  0001 C CNN
	1    2300 2400
	1    0    0    -1  
$EndComp
$Comp
L Device:C_Polarized_Small_US C1
U 1 1 60629DFA
P 2500 2050
F 0 "C1" H 2591 2096 50  0000 L CNN
F 1 "100uF" H 2591 2005 50  0000 L CNN
F 2 "Capacitor_THT:CP_Radial_D5.0mm_P2.50mm" H 2500 2050 50  0001 C CNN
F 3 "~" H 2500 2050 50  0001 C CNN
	1    2500 2050
	1    0    0    -1  
$EndComp
Wire Wire Line
	2500 2150 2300 2200
Connection ~ 2300 2200
$Comp
L Device:C_Small C2
U 1 1 6062AEB3
P 2950 2100
F 0 "C2" H 3042 2146 50  0000 L CNN
F 1 ".1uF" H 3042 2055 50  0000 L CNN
F 2 "Capacitor_THT:C_Disc_D3.8mm_W2.6mm_P2.50mm" H 2950 2100 50  0001 C CNN
F 3 "~" H 2950 2100 50  0001 C CNN
	1    2950 2100
	1    0    0    -1  
$EndComp
Wire Wire Line
	2950 2000 2500 1950
Wire Wire Line
	2950 2200 2500 2150
Connection ~ 2500 2150
$Comp
L Regulator_Linear:L7805 U1
U 1 1 6062B717
P 3600 2000
F 0 "U1" H 3600 2242 50  0000 C CNN
F 1 "L7805" H 3600 2151 50  0000 C CNN
F 2 "Package_TO_SOT_THT:TO-220-3_Vertical" H 3625 1850 50  0001 L CIN
F 3 "http://www.st.com/content/ccc/resource/technical/document/datasheet/41/4f/b3/b0/12/d4/47/88/CD00000444.pdf/files/CD00000444.pdf/jcr:content/translations/en.CD00000444.pdf" H 3600 1950 50  0001 C CNN
	1    3600 2000
	1    0    0    -1  
$EndComp
Wire Wire Line
	3300 2000 2950 2000
Connection ~ 2950 2000
Wire Wire Line
	3600 2300 3600 2350
$Comp
L power:GND #PWR0103
U 1 1 6062C90C
P 3600 2400
F 0 "#PWR0103" H 3600 2150 50  0001 C CNN
F 1 "GND" H 3605 2227 50  0000 C CNN
F 2 "" H 3600 2400 50  0001 C CNN
F 3 "" H 3600 2400 50  0001 C CNN
	1    3600 2400
	1    0    0    -1  
$EndComp
$Comp
L Device:C_Polarized_Small_US C3
U 1 1 6062CB58
P 4200 2150
F 0 "C3" H 4291 2196 50  0000 L CNN
F 1 "100uF" H 4291 2105 50  0000 L CNN
F 2 "Capacitor_THT:CP_Radial_D5.0mm_P2.50mm" H 4200 2150 50  0001 C CNN
F 3 "~" H 4200 2150 50  0001 C CNN
	1    4200 2150
	1    0    0    -1  
$EndComp
Wire Wire Line
	3900 2000 4200 2000
Wire Wire Line
	4200 2000 4200 2050
Wire Wire Line
	4200 2250 4200 2350
Wire Wire Line
	4200 2350 3600 2350
Connection ~ 3600 2350
Wire Wire Line
	3600 2350 3600 2400
Connection ~ 4200 2000
$Comp
L power:+5V #PWR0104
U 1 1 6062E037
P 4200 1800
F 0 "#PWR0104" H 4200 1650 50  0001 C CNN
F 1 "+5V" H 4215 1973 50  0000 C CNN
F 2 "" H 4200 1800 50  0001 C CNN
F 3 "" H 4200 1800 50  0001 C CNN
	1    4200 1800
	1    0    0    -1  
$EndComp
Wire Wire Line
	4200 1800 4200 2000
$Comp
L 74xx:74AHC04 U2
U 1 1 6063DD10
P 4600 3150
F 0 "U2" H 4600 3467 50  0000 C CNN
F 1 "74AHC04" H 4600 3376 50  0000 C CNN
F 2 "Package_DIP:DIP-14_W7.62mm_Socket_LongPads" H 4600 3150 50  0001 C CNN
F 3 "https://assets.nexperia.com/documents/data-sheet/74AHC_AHCT04.pdf" H 4600 3150 50  0001 C CNN
	1    4600 3150
	1    0    0    -1  
$EndComp
$Comp
L 74xx:74AHC04 U2
U 2 1 6063F95A
P 7050 3150
F 0 "U2" H 7050 3467 50  0000 C CNN
F 1 "74AHC04" H 7050 3376 50  0000 C CNN
F 2 "Package_DIP:DIP-14_W7.62mm_Socket_LongPads" H 7050 3150 50  0001 C CNN
F 3 "https://assets.nexperia.com/documents/data-sheet/74AHC_AHCT04.pdf" H 7050 3150 50  0001 C CNN
	2    7050 3150
	1    0    0    -1  
$EndComp
$Comp
L 74xx:74AHC04 U2
U 5 1 60642FD7
P 1650 6200
F 0 "U2" H 1650 6517 50  0000 C CNN
F 1 "74AHC04" H 1650 6426 50  0000 C CNN
F 2 "Package_DIP:DIP-14_W7.62mm_Socket_LongPads" H 1650 6200 50  0001 C CNN
F 3 "https://assets.nexperia.com/documents/data-sheet/74AHC_AHCT04.pdf" H 1650 6200 50  0001 C CNN
	5    1650 6200
	1    0    0    -1  
$EndComp
$Comp
L 74xx:74AHC04 U2
U 6 1 60644802
P 1650 6800
F 0 "U2" H 1650 7117 50  0000 C CNN
F 1 "74AHC04" H 1650 7026 50  0000 C CNN
F 2 "Package_DIP:DIP-14_W7.62mm_Socket_LongPads" H 1650 6800 50  0001 C CNN
F 3 "https://assets.nexperia.com/documents/data-sheet/74AHC_AHCT04.pdf" H 1650 6800 50  0001 C CNN
	6    1650 6800
	1    0    0    -1  
$EndComp
$Comp
L 74xx:74AHC04 U2
U 7 1 606452E5
P 4800 1450
F 0 "U2" H 4900 1800 50  0000 L CNN
F 1 "74AHC04" V 5050 1300 50  0000 L CNN
F 2 "Package_DIP:DIP-14_W7.62mm_Socket_LongPads" H 4800 1450 50  0001 C CNN
F 3 "https://assets.nexperia.com/documents/data-sheet/74AHC_AHCT04.pdf" H 4800 1450 50  0001 C CNN
	7    4800 1450
	1    0    0    -1  
$EndComp
Wire Wire Line
	4800 950  4800 850 
Wire Wire Line
	4800 1950 4800 2050
$Comp
L power:+5V #PWR0105
U 1 1 60646E75
P 5150 850
F 0 "#PWR0105" H 5150 700 50  0001 C CNN
F 1 "+5V" H 5165 1023 50  0000 C CNN
F 2 "" H 5150 850 50  0001 C CNN
F 3 "" H 5150 850 50  0001 C CNN
	1    5150 850 
	1    0    0    -1  
$EndComp
$Comp
L power:GND #PWR0106
U 1 1 60647417
P 5200 2100
F 0 "#PWR0106" H 5200 1850 50  0001 C CNN
F 1 "GND" H 5205 1927 50  0000 C CNN
F 2 "" H 5200 2100 50  0001 C CNN
F 3 "" H 5200 2100 50  0001 C CNN
	1    5200 2100
	1    0    0    -1  
$EndComp
$Comp
L Connector:userportc64 CN1
U 1 1 6065AE49
P 2650 3950
F 0 "CN1" H 2650 4715 50  0000 C CNN
F 1 "userportc64" H 2650 4624 50  0000 C CNN
F 2 "Connector_PinHeader_2.54mm:PinHeader_2x12_P2.54mm_Horizontal" H 2450 4650 50  0001 C CNN
F 3 "" H 2450 4650 50  0001 C CNN
	1    2650 3950
	1    0    0    -1  
$EndComp
Wire Wire Line
	3050 3650 3250 3650
Wire Wire Line
	3050 3750 3250 3750
Wire Wire Line
	3050 3850 3250 3850
Wire Wire Line
	3050 3950 3250 3950
Wire Wire Line
	3050 4050 3250 4050
Wire Wire Line
	3050 4150 3250 4150
Wire Wire Line
	3050 4250 3250 4250
Wire Wire Line
	3050 4350 3250 4350
Wire Wire Line
	3050 4450 3250 4450
Wire Wire Line
	3050 4550 3150 4550
Wire Wire Line
	3150 4550 3150 4650
$Comp
L power:GND #PWR0107
U 1 1 606601B2
P 3150 4650
F 0 "#PWR0107" H 3150 4400 50  0001 C CNN
F 1 "GND" H 3155 4477 50  0000 C CNN
F 2 "" H 3150 4650 50  0001 C CNN
F 3 "" H 3150 4650 50  0001 C CNN
	1    3150 4650
	1    0    0    -1  
$EndComp
Wire Wire Line
	2250 4550 2150 4550
Wire Wire Line
	2150 4550 2150 4650
$Comp
L power:GND #PWR0108
U 1 1 60661000
P 2150 4650
F 0 "#PWR0108" H 2150 4400 50  0001 C CNN
F 1 "GND" H 2155 4477 50  0000 C CNN
F 2 "" H 2150 4650 50  0001 C CNN
F 3 "" H 2150 4650 50  0001 C CNN
	1    2150 4650
	1    0    0    -1  
$EndComp
Wire Wire Line
	2250 3450 2150 3450
Wire Wire Line
	2150 3450 2150 3350
Wire Wire Line
	3050 3450 3200 3450
Wire Wire Line
	3200 3450 3200 3350
$Comp
L power:GND #PWR0109
U 1 1 6066292C
P 3200 3350
F 0 "#PWR0109" H 3200 3100 50  0001 C CNN
F 1 "GND" H 3205 3177 50  0000 C CNN
F 2 "" H 3200 3350 50  0001 C CNN
F 3 "" H 3200 3350 50  0001 C CNN
	1    3200 3350
	-1   0    0    1   
$EndComp
$Comp
L power:GND #PWR0110
U 1 1 60662EDC
P 2150 3350
F 0 "#PWR0110" H 2150 3100 50  0001 C CNN
F 1 "GND" H 2155 3177 50  0000 C CNN
F 2 "" H 2150 3350 50  0001 C CNN
F 3 "" H 2150 3350 50  0001 C CNN
	1    2150 3350
	-1   0    0    1   
$EndComp
Wire Wire Line
	4300 3150 4100 3150
Wire Wire Line
	6750 3150 6550 3150
Text Label 3250 3650 2    50   ~ 0
PB0
Text Label 3250 3750 2    50   ~ 0
PB1
Text Label 3250 3850 2    50   ~ 0
PB2
Text Label 3250 3950 2    50   ~ 0
PB3
Text Label 3250 4050 2    50   ~ 0
PB4
Text Label 3250 4150 2    50   ~ 0
PB5
Text Label 3250 4250 2    50   ~ 0
PB6
Text Label 3250 4350 2    50   ~ 0
PB7
Text Label 4100 3150 0    50   ~ 0
PB0
Text Label 6550 3150 0    50   ~ 0
PB1
$Comp
L 74xx:74AHC04 U3
U 1 1 6066E07E
P 4600 4900
F 0 "U3" H 4600 5217 50  0000 C CNN
F 1 "74AHC04" H 4600 5126 50  0000 C CNN
F 2 "Package_DIP:DIP-14_W7.62mm_Socket_LongPads" H 4600 4900 50  0001 C CNN
F 3 "https://assets.nexperia.com/documents/data-sheet/74AHC_AHCT04.pdf" H 4600 4900 50  0001 C CNN
	1    4600 4900
	1    0    0    -1  
$EndComp
$Comp
L 74xx:74AHC04 U3
U 2 1 60671264
P 7000 4900
F 0 "U3" H 7000 5217 50  0000 C CNN
F 1 "74AHC04" H 7000 5126 50  0000 C CNN
F 2 "Package_DIP:DIP-14_W7.62mm_Socket_LongPads" H 7000 4900 50  0001 C CNN
F 3 "https://assets.nexperia.com/documents/data-sheet/74AHC_AHCT04.pdf" H 7000 4900 50  0001 C CNN
	2    7000 4900
	1    0    0    -1  
$EndComp
$Comp
L 74xx:74AHC04 U3
U 3 1 6067221B
P 4600 5750
F 0 "U3" H 4600 6067 50  0000 C CNN
F 1 "74AHC04" H 4600 5976 50  0000 C CNN
F 2 "Package_DIP:DIP-14_W7.62mm_Socket_LongPads" H 4600 5750 50  0001 C CNN
F 3 "https://assets.nexperia.com/documents/data-sheet/74AHC_AHCT04.pdf" H 4600 5750 50  0001 C CNN
	3    4600 5750
	1    0    0    -1  
$EndComp
$Comp
L 74xx:74AHC04 U3
U 4 1 60673535
P 7000 5750
F 0 "U3" H 7000 6067 50  0000 C CNN
F 1 "74AHC04" H 7000 5976 50  0000 C CNN
F 2 "Package_DIP:DIP-14_W7.62mm_Socket_LongPads" H 7000 5750 50  0001 C CNN
F 3 "https://assets.nexperia.com/documents/data-sheet/74AHC_AHCT04.pdf" H 7000 5750 50  0001 C CNN
	4    7000 5750
	1    0    0    -1  
$EndComp
Wire Wire Line
	4300 4900 4100 4900
Wire Wire Line
	6700 4900 6500 4900
Wire Wire Line
	4300 5750 4100 5750
Wire Wire Line
	6700 5750 6500 5750
Text Label 4100 4900 0    50   ~ 0
PB4
Text Label 6500 4900 0    50   ~ 0
PB5
Text Label 4100 5750 0    50   ~ 0
PB6
Text Label 6500 5750 0    50   ~ 0
PB7
$Comp
L 74xx:74AHC04 U3
U 5 1 6067DCF5
P 2400 6200
F 0 "U3" H 2400 6517 50  0000 C CNN
F 1 "74AHC04" H 2400 6426 50  0000 C CNN
F 2 "Package_DIP:DIP-14_W7.62mm_Socket_LongPads" H 2400 6200 50  0001 C CNN
F 3 "https://assets.nexperia.com/documents/data-sheet/74AHC_AHCT04.pdf" H 2400 6200 50  0001 C CNN
	5    2400 6200
	1    0    0    -1  
$EndComp
$Comp
L 74xx:74AHC04 U3
U 6 1 6067ED29
P 2400 6800
F 0 "U3" H 2400 7117 50  0000 C CNN
F 1 "74AHC04" H 2400 7026 50  0000 C CNN
F 2 "Package_DIP:DIP-14_W7.62mm_Socket_LongPads" H 2400 6800 50  0001 C CNN
F 3 "https://assets.nexperia.com/documents/data-sheet/74AHC_AHCT04.pdf" H 2400 6800 50  0001 C CNN
	6    2400 6800
	1    0    0    -1  
$EndComp
$Comp
L 74xx:74AHC04 U3
U 7 1 6067F894
P 5600 1450
F 0 "U3" H 5700 1800 50  0000 L CNN
F 1 "74AHC04" V 5850 1300 50  0000 L CNN
F 2 "Package_DIP:DIP-14_W7.62mm_Socket_LongPads" H 5600 1450 50  0001 C CNN
F 3 "https://assets.nexperia.com/documents/data-sheet/74AHC_AHCT04.pdf" H 5600 1450 50  0001 C CNN
	7    5600 1450
	1    0    0    -1  
$EndComp
Wire Wire Line
	5600 2050 5600 1950
Wire Wire Line
	4800 850  5150 850 
Wire Wire Line
	5600 850  5600 950 
Connection ~ 5150 850 
Wire Wire Line
	5150 850  5600 850 
$Comp
L Device:C_Small C5
U 1 1 606852C5
P 4100 950
F 0 "C5" H 4192 996 50  0000 L CNN
F 1 ".1 uF" H 4192 905 50  0000 L CNN
F 2 "Capacitor_THT:C_Disc_D3.8mm_W2.6mm_P2.50mm" H 4100 950 50  0001 C CNN
F 3 "~" H 4100 950 50  0001 C CNN
	1    4100 950 
	1    0    0    -1  
$EndComp
$Comp
L Device:C_Small C4
U 1 1 606854C6
P 3700 950
F 0 "C4" H 3792 996 50  0000 L CNN
F 1 ".1 uF" H 3792 905 50  0000 L CNN
F 2 "Capacitor_THT:C_Disc_D3.8mm_W2.6mm_P2.50mm" H 3700 950 50  0001 C CNN
F 3 "~" H 3700 950 50  0001 C CNN
	1    3700 950 
	1    0    0    -1  
$EndComp
Wire Wire Line
	3700 850  3700 800 
Wire Wire Line
	3700 800  3900 800 
Wire Wire Line
	4100 800  4100 850 
Wire Wire Line
	4100 1050 4100 1100
Wire Wire Line
	4100 1100 3900 1100
Wire Wire Line
	3700 1100 3700 1050
Wire Wire Line
	3900 1100 3900 1150
Connection ~ 3900 1100
Wire Wire Line
	3900 1100 3700 1100
Wire Wire Line
	3900 800  3900 750 
Connection ~ 3900 800 
Wire Wire Line
	3900 800  4100 800 
$Comp
L power:+5V #PWR0111
U 1 1 6068AFDA
P 3900 750
F 0 "#PWR0111" H 3900 600 50  0001 C CNN
F 1 "+5V" H 3915 923 50  0000 C CNN
F 2 "" H 3900 750 50  0001 C CNN
F 3 "" H 3900 750 50  0001 C CNN
	1    3900 750 
	1    0    0    -1  
$EndComp
$Comp
L power:GND #PWR0112
U 1 1 6068B829
P 3900 1150
F 0 "#PWR0112" H 3900 900 50  0001 C CNN
F 1 "GND" H 3905 977 50  0000 C CNN
F 2 "" H 3900 1150 50  0001 C CNN
F 3 "" H 3900 1150 50  0001 C CNN
	1    3900 1150
	1    0    0    -1  
$EndComp
Wire Wire Line
	4800 2050 5200 2050
Connection ~ 5200 2050
Wire Wire Line
	5200 2050 5600 2050
Wire Wire Line
	5200 2050 5200 2100
Text Label 5150 3150 2    50   ~ 0
GR1A
Text Label 7600 3150 2    50   ~ 0
YE1A
Text Label 5150 4900 2    50   ~ 0
GR1B
Text Label 7550 4900 2    50   ~ 0
YE1B
Text Label 5150 5750 2    50   ~ 0
GR2B
Text Label 7550 5750 2    50   ~ 0
YE2B
$Comp
L Transistor_BJT:PN2222A Q1
U 1 1 608B0CAF
P 5800 3150
F 0 "Q1" H 5990 3196 50  0000 L CNN
F 1 "PN2222A" H 5990 3105 50  0000 L CNN
F 2 "Package_TO_SOT_THT:TO-92_Inline" H 6000 3075 50  0001 L CIN
F 3 "https://www.onsemi.com/pub/Collateral/PN2222-D.PDF" H 5800 3150 50  0001 L CNN
	1    5800 3150
	1    0    0    -1  
$EndComp
$Comp
L Device:R_Small_US R1
U 1 1 608B1FDA
P 5400 3150
F 0 "R1" V 5195 3150 50  0000 C CNN
F 1 "1K" V 5286 3150 50  0000 C CNN
F 2 "Resistor_THT:R_Axial_DIN0207_L6.3mm_D2.5mm_P10.16mm_Horizontal" H 5400 3150 50  0001 C CNN
F 3 "~" H 5400 3150 50  0001 C CNN
	1    5400 3150
	0    1    1    0   
$EndComp
Wire Wire Line
	5500 3150 5600 3150
Wire Wire Line
	5900 3350 5900 3400
$Comp
L power:GND #PWR0113
U 1 1 608B82F4
P 5900 3400
F 0 "#PWR0113" H 5900 3150 50  0001 C CNN
F 1 "GND" H 5905 3227 50  0000 C CNN
F 2 "" H 5900 3400 50  0001 C CNN
F 3 "" H 5900 3400 50  0001 C CNN
	1    5900 3400
	1    0    0    -1  
$EndComp
Wire Wire Line
	5900 2950 5900 2900
Wire Wire Line
	5900 2900 6200 2900
$Comp
L Transistor_BJT:PN2222A Q2
U 1 1 608BEE34
P 8250 3150
F 0 "Q2" H 8440 3196 50  0000 L CNN
F 1 "PN2222A" H 8440 3105 50  0000 L CNN
F 2 "Package_TO_SOT_THT:TO-92_Inline" H 8450 3075 50  0001 L CIN
F 3 "https://www.onsemi.com/pub/Collateral/PN2222-D.PDF" H 8250 3150 50  0001 L CNN
	1    8250 3150
	1    0    0    -1  
$EndComp
$Comp
L Device:R_Small_US R2
U 1 1 608BF35C
P 7850 3150
F 0 "R2" V 7645 3150 50  0000 C CNN
F 1 "1K" V 7736 3150 50  0000 C CNN
F 2 "Resistor_THT:R_Axial_DIN0207_L6.3mm_D2.5mm_P10.16mm_Horizontal" H 7850 3150 50  0001 C CNN
F 3 "~" H 7850 3150 50  0001 C CNN
	1    7850 3150
	0    1    1    0   
$EndComp
Wire Wire Line
	7950 3150 8050 3150
Wire Wire Line
	8350 3350 8350 3400
$Comp
L power:GND #PWR0114
U 1 1 608BF369
P 8350 3400
F 0 "#PWR0114" H 8350 3150 50  0001 C CNN
F 1 "GND" H 8355 3227 50  0000 C CNN
F 2 "" H 8350 3400 50  0001 C CNN
F 3 "" H 8350 3400 50  0001 C CNN
	1    8350 3400
	1    0    0    -1  
$EndComp
Wire Wire Line
	8350 2950 8350 2900
Wire Wire Line
	8350 2900 8650 2900
Wire Wire Line
	5900 3800 6200 3800
$Comp
L Transistor_BJT:PN2222A Q5
U 1 1 608D15F7
P 5800 4900
F 0 "Q5" H 5990 4946 50  0000 L CNN
F 1 "PN2222A" H 5990 4855 50  0000 L CNN
F 2 "Package_TO_SOT_THT:TO-92_Inline" H 6000 4825 50  0001 L CIN
F 3 "https://www.onsemi.com/pub/Collateral/PN2222-D.PDF" H 5800 4900 50  0001 L CNN
	1    5800 4900
	1    0    0    -1  
$EndComp
$Comp
L Device:R_Small_US R5
U 1 1 608D1BD3
P 5400 4900
F 0 "R5" V 5195 4900 50  0000 C CNN
F 1 "1K" V 5286 4900 50  0000 C CNN
F 2 "Resistor_THT:R_Axial_DIN0207_L6.3mm_D2.5mm_P10.16mm_Horizontal" H 5400 4900 50  0001 C CNN
F 3 "~" H 5400 4900 50  0001 C CNN
	1    5400 4900
	0    1    1    0   
$EndComp
Wire Wire Line
	5500 4900 5600 4900
Wire Wire Line
	5900 5100 5900 5150
$Comp
L power:GND #PWR0115
U 1 1 608D1BE0
P 5900 5150
F 0 "#PWR0115" H 5900 4900 50  0001 C CNN
F 1 "GND" H 5905 4977 50  0000 C CNN
F 2 "" H 5900 5150 50  0001 C CNN
F 3 "" H 5900 5150 50  0001 C CNN
	1    5900 5150
	1    0    0    -1  
$EndComp
Wire Wire Line
	5900 4700 5900 4650
Wire Wire Line
	5900 4650 6200 4650
$Comp
L Transistor_BJT:PN2222A Q6
U 1 1 608D1BED
P 8200 4900
F 0 "Q6" H 8390 4946 50  0000 L CNN
F 1 "PN2222A" H 8390 4855 50  0000 L CNN
F 2 "Package_TO_SOT_THT:TO-92_Inline" H 8400 4825 50  0001 L CIN
F 3 "https://www.onsemi.com/pub/Collateral/PN2222-D.PDF" H 8200 4900 50  0001 L CNN
	1    8200 4900
	1    0    0    -1  
$EndComp
$Comp
L Device:R_Small_US R6
U 1 1 608D1BF7
P 7800 4900
F 0 "R6" V 7595 4900 50  0000 C CNN
F 1 "1K" V 7686 4900 50  0000 C CNN
F 2 "Resistor_THT:R_Axial_DIN0207_L6.3mm_D2.5mm_P10.16mm_Horizontal" H 7800 4900 50  0001 C CNN
F 3 "~" H 7800 4900 50  0001 C CNN
	1    7800 4900
	0    1    1    0   
$EndComp
Wire Wire Line
	7900 4900 8000 4900
Wire Wire Line
	8300 5100 8300 5150
$Comp
L power:GND #PWR0116
U 1 1 608D1C04
P 8300 5150
F 0 "#PWR0116" H 8300 4900 50  0001 C CNN
F 1 "GND" H 8305 4977 50  0000 C CNN
F 2 "" H 8300 5150 50  0001 C CNN
F 3 "" H 8300 5150 50  0001 C CNN
	1    8300 5150
	1    0    0    -1  
$EndComp
Wire Wire Line
	8300 4700 8300 4650
Wire Wire Line
	8300 4650 8600 4650
$Comp
L Transistor_BJT:PN2222A Q7
U 1 1 608D1C11
P 5800 5750
F 0 "Q7" H 5990 5796 50  0000 L CNN
F 1 "PN2222A" H 5990 5705 50  0000 L CNN
F 2 "Package_TO_SOT_THT:TO-92_Inline" H 6000 5675 50  0001 L CIN
F 3 "https://www.onsemi.com/pub/Collateral/PN2222-D.PDF" H 5800 5750 50  0001 L CNN
	1    5800 5750
	1    0    0    -1  
$EndComp
$Comp
L Device:R_Small_US R7
U 1 1 608D1C1B
P 5400 5750
F 0 "R7" V 5195 5750 50  0000 C CNN
F 1 "1K" V 5286 5750 50  0000 C CNN
F 2 "Resistor_THT:R_Axial_DIN0207_L6.3mm_D2.5mm_P10.16mm_Horizontal" H 5400 5750 50  0001 C CNN
F 3 "~" H 5400 5750 50  0001 C CNN
	1    5400 5750
	0    1    1    0   
$EndComp
Wire Wire Line
	5500 5750 5600 5750
Wire Wire Line
	5900 5950 5900 6000
$Comp
L power:GND #PWR0117
U 1 1 608D1C28
P 5900 6000
F 0 "#PWR0117" H 5900 5750 50  0001 C CNN
F 1 "GND" H 5905 5827 50  0000 C CNN
F 2 "" H 5900 6000 50  0001 C CNN
F 3 "" H 5900 6000 50  0001 C CNN
	1    5900 6000
	1    0    0    -1  
$EndComp
Wire Wire Line
	5900 5550 5900 5500
$Comp
L Transistor_BJT:PN2222A Q8
U 1 1 608D1C35
P 8200 5750
F 0 "Q8" H 8390 5796 50  0000 L CNN
F 1 "PN2222A" H 8390 5705 50  0000 L CNN
F 2 "Package_TO_SOT_THT:TO-92_Inline" H 8400 5675 50  0001 L CIN
F 3 "https://www.onsemi.com/pub/Collateral/PN2222-D.PDF" H 8200 5750 50  0001 L CNN
	1    8200 5750
	1    0    0    -1  
$EndComp
$Comp
L Device:R_Small_US R8
U 1 1 608D1C3F
P 7800 5750
F 0 "R8" V 7595 5750 50  0000 C CNN
F 1 "1K" V 7686 5750 50  0000 C CNN
F 2 "Resistor_THT:R_Axial_DIN0207_L6.3mm_D2.5mm_P10.16mm_Horizontal" H 7800 5750 50  0001 C CNN
F 3 "~" H 7800 5750 50  0001 C CNN
	1    7800 5750
	0    1    1    0   
$EndComp
Wire Wire Line
	7900 5750 8000 5750
Wire Wire Line
	8300 5950 8300 6000
$Comp
L power:GND #PWR0118
U 1 1 608D1C4C
P 8300 6000
F 0 "#PWR0118" H 8300 5750 50  0001 C CNN
F 1 "GND" H 8305 5827 50  0000 C CNN
F 2 "" H 8300 6000 50  0001 C CNN
F 3 "" H 8300 6000 50  0001 C CNN
	1    8300 6000
	1    0    0    -1  
$EndComp
Wire Wire Line
	8300 5550 8300 5500
Wire Wire Line
	4900 4900 5300 4900
Wire Wire Line
	7300 4900 7700 4900
Wire Wire Line
	4900 5750 5300 5750
Wire Wire Line
	7300 5750 7700 5750
Wire Wire Line
	4900 3150 5300 3150
Wire Wire Line
	7350 3150 7750 3150
Text Label 6200 2900 2    50   ~ 0
GREY1A
Text Label 8650 2900 2    50   ~ 0
YELL1A
Text Label 6200 3800 2    50   ~ 0
GREY2A
Wire Wire Line
	7300 4050 7700 4050
Wire Wire Line
	4900 4050 5300 4050
Wire Wire Line
	8300 3800 8600 3800
Wire Wire Line
	8300 3850 8300 3800
$Comp
L power:GND #PWR0119
U 1 1 608C29EA
P 8300 4300
F 0 "#PWR0119" H 8300 4050 50  0001 C CNN
F 1 "GND" H 8305 4127 50  0000 C CNN
F 2 "" H 8300 4300 50  0001 C CNN
F 3 "" H 8300 4300 50  0001 C CNN
	1    8300 4300
	1    0    0    -1  
$EndComp
Wire Wire Line
	8300 4250 8300 4300
Wire Wire Line
	7900 4050 8000 4050
$Comp
L Device:R_Small_US R4
U 1 1 608C29DD
P 7800 4050
F 0 "R4" V 7595 4050 50  0000 C CNN
F 1 "1K" V 7686 4050 50  0000 C CNN
F 2 "Resistor_THT:R_Axial_DIN0207_L6.3mm_D2.5mm_P10.16mm_Horizontal" H 7800 4050 50  0001 C CNN
F 3 "~" H 7800 4050 50  0001 C CNN
	1    7800 4050
	0    1    1    0   
$EndComp
$Comp
L Transistor_BJT:PN2222A Q4
U 1 1 608C29D3
P 8200 4050
F 0 "Q4" H 8390 4096 50  0000 L CNN
F 1 "PN2222A" H 8390 4005 50  0000 L CNN
F 2 "Package_TO_SOT_THT:TO-92_Inline" H 8400 3975 50  0001 L CIN
F 3 "https://www.onsemi.com/pub/Collateral/PN2222-D.PDF" H 8200 4050 50  0001 L CNN
	1    8200 4050
	1    0    0    -1  
$EndComp
Wire Wire Line
	5900 3850 5900 3800
$Comp
L power:GND #PWR0120
U 1 1 608C29C6
P 5900 4300
F 0 "#PWR0120" H 5900 4050 50  0001 C CNN
F 1 "GND" H 5905 4127 50  0000 C CNN
F 2 "" H 5900 4300 50  0001 C CNN
F 3 "" H 5900 4300 50  0001 C CNN
	1    5900 4300
	1    0    0    -1  
$EndComp
Wire Wire Line
	5900 4250 5900 4300
Wire Wire Line
	5500 4050 5600 4050
$Comp
L Device:R_Small_US R3
U 1 1 608C29B9
P 5400 4050
F 0 "R3" V 5195 4050 50  0000 C CNN
F 1 "1K" V 5286 4050 50  0000 C CNN
F 2 "Resistor_THT:R_Axial_DIN0207_L6.3mm_D2.5mm_P10.16mm_Horizontal" H 5400 4050 50  0001 C CNN
F 3 "~" H 5400 4050 50  0001 C CNN
	1    5400 4050
	0    1    1    0   
$EndComp
$Comp
L Transistor_BJT:PN2222A Q3
U 1 1 608C2455
P 5800 4050
F 0 "Q3" H 5990 4096 50  0000 L CNN
F 1 "PN2222A" H 5990 4005 50  0000 L CNN
F 2 "Package_TO_SOT_THT:TO-92_Inline" H 6000 3975 50  0001 L CIN
F 3 "https://www.onsemi.com/pub/Collateral/PN2222-D.PDF" H 5800 4050 50  0001 L CNN
	1    5800 4050
	1    0    0    -1  
$EndComp
Text Label 7550 4050 2    50   ~ 0
YEL2A
Text Label 5150 4050 2    50   ~ 0
GR2A
Text Label 6500 4050 0    50   ~ 0
PB3
Text Label 4100 4050 0    50   ~ 0
PB2
Wire Wire Line
	6700 4050 6500 4050
Wire Wire Line
	4300 4050 4100 4050
$Comp
L 74xx:74AHC04 U2
U 4 1 60641B18
P 7000 4050
F 0 "U2" H 7000 4367 50  0000 C CNN
F 1 "74AHC04" H 7000 4276 50  0000 C CNN
F 2 "Package_DIP:DIP-14_W7.62mm_Socket_LongPads" H 7000 4050 50  0001 C CNN
F 3 "https://assets.nexperia.com/documents/data-sheet/74AHC_AHCT04.pdf" H 7000 4050 50  0001 C CNN
	4    7000 4050
	1    0    0    -1  
$EndComp
$Comp
L 74xx:74AHC04 U2
U 3 1 60640B4E
P 4600 4050
F 0 "U2" H 4600 4367 50  0000 C CNN
F 1 "74AHC04" H 4600 4276 50  0000 C CNN
F 2 "Package_DIP:DIP-14_W7.62mm_Socket_LongPads" H 4600 4050 50  0001 C CNN
F 3 "https://assets.nexperia.com/documents/data-sheet/74AHC_AHCT04.pdf" H 4600 4050 50  0001 C CNN
	3    4600 4050
	1    0    0    -1  
$EndComp
Text Label 8600 3800 2    50   ~ 0
YELL2A
Wire Wire Line
	5900 5500 6200 5500
Wire Wire Line
	8300 5500 8600 5500
Text Label 6200 4650 2    50   ~ 0
GREY1B
Text Label 8600 4650 2    50   ~ 0
YELL1B
Text Label 6200 5500 2    50   ~ 0
GREY2B
Text Label 8600 5500 2    50   ~ 0
YELL2B
Wire Notes Line
	1200 5750 2850 5750
Wire Notes Line
	2850 5750 2850 7150
Wire Notes Line
	2850 7150 1200 7150
Wire Notes Line
	1200 7150 1200 5750
Text Notes 2200 5700 2    50   ~ 0
NOT USED
Wire Wire Line
	10100 4700 10000 4700
Wire Wire Line
	10000 4900 10100 4900
Wire Wire Line
	10000 4700 10000 4600
$Comp
L power:+12V #PWR0121
U 1 1 609C53DE
P 10000 4600
F 0 "#PWR0121" H 10000 4450 50  0001 C CNN
F 1 "+12V" H 10015 4773 50  0000 C CNN
F 2 "" H 10000 4600 50  0001 C CNN
F 3 "" H 10000 4600 50  0001 C CNN
	1    10000 4600
	1    0    0    -1  
$EndComp
Wire Wire Line
	10100 3250 9800 3250
Wire Wire Line
	10100 3350 9800 3350
Wire Wire Line
	10100 3450 9800 3450
Wire Wire Line
	10100 3550 9800 3550
Wire Wire Line
	10100 3850 9800 3850
Wire Wire Line
	10100 3950 9800 3950
Wire Wire Line
	10100 4050 9800 4050
Wire Wire Line
	10100 4150 9800 4150
Text Label 9800 3250 0    50   ~ 0
GREY1A
Text Label 9800 3350 0    50   ~ 0
YELL1A
Text Label 9800 3450 0    50   ~ 0
GREY2A
Text Label 9800 3550 0    50   ~ 0
YELL2A
Text Label 9800 3850 0    50   ~ 0
GREY1B
Text Label 9800 3950 0    50   ~ 0
YELL1B
Text Label 9800 4050 0    50   ~ 0
GREY2B
Text Label 9800 4150 0    50   ~ 0
YELL2B
$Comp
L power:+5VP #PWR0122
U 1 1 60A107FB
P 1900 3350
F 0 "#PWR0122" H 1900 3200 50  0001 C CNN
F 1 "+5VP" H 1915 3523 50  0000 C CNN
F 2 "" H 1900 3350 50  0001 C CNN
F 3 "" H 1900 3350 50  0001 C CNN
	1    1900 3350
	1    0    0    -1  
$EndComp
Wire Wire Line
	1900 3350 1900 3550
Wire Wire Line
	1900 3550 2250 3550
$Comp
L Device:R_Small_US R9
U 1 1 60A149EE
P 1900 3750
F 0 "R9" H 1832 3704 50  0000 R CNN
F 1 "1K" H 1832 3795 50  0000 R CNN
F 2 "Resistor_THT:R_Axial_DIN0207_L6.3mm_D2.5mm_P10.16mm_Horizontal" H 1900 3750 50  0001 C CNN
F 3 "~" H 1900 3750 50  0001 C CNN
	1    1900 3750
	-1   0    0    1   
$EndComp
Wire Wire Line
	1900 3650 1900 3550
Connection ~ 1900 3550
Wire Wire Line
	1900 3850 1900 3900
$Comp
L Device:LED D1
U 1 1 60A1CAB1
P 1900 4050
F 0 "D1" V 1847 4130 50  0000 L CNN
F 1 "LED" V 1938 4130 50  0000 L CNN
F 2 "LED_THT:LED_D3.0mm" H 1900 4050 50  0001 C CNN
F 3 "~" H 1900 4050 50  0001 C CNN
	1    1900 4050
	0    1    1    0   
$EndComp
Wire Wire Line
	1900 4200 1900 4550
Wire Wire Line
	1900 4550 2150 4550
Connection ~ 2150 4550
$Comp
L Device:R_Small_US R10
U 1 1 60A2182F
P 6300 1150
F 0 "R10" H 6232 1104 50  0000 R CNN
F 1 "220" H 6232 1195 50  0000 R CNN
F 2 "Resistor_THT:R_Axial_DIN0207_L6.3mm_D2.5mm_P10.16mm_Horizontal" H 6300 1150 50  0001 C CNN
F 3 "~" H 6300 1150 50  0001 C CNN
	1    6300 1150
	-1   0    0    1   
$EndComp
$Comp
L Device:LED D2
U 1 1 60A21FC5
P 6300 1550
F 0 "D2" V 6247 1630 50  0000 L CNN
F 1 "LED" V 6338 1630 50  0000 L CNN
F 2 "LED_THT:LED_D3.0mm" H 6300 1550 50  0001 C CNN
F 3 "~" H 6300 1550 50  0001 C CNN
	1    6300 1550
	0    1    1    0   
$EndComp
Wire Wire Line
	6300 1400 6300 1250
Wire Wire Line
	6300 1700 6300 1900
Wire Wire Line
	6300 1050 6300 950 
$Comp
L power:+5V #PWR0123
U 1 1 60A2DE89
P 6300 950
F 0 "#PWR0123" H 6300 800 50  0001 C CNN
F 1 "+5V" H 6315 1123 50  0000 C CNN
F 2 "" H 6300 950 50  0001 C CNN
F 3 "" H 6300 950 50  0001 C CNN
	1    6300 950 
	1    0    0    -1  
$EndComp
$Comp
L power:GND #PWR0124
U 1 1 60A2E32D
P 6300 1900
F 0 "#PWR0124" H 6300 1650 50  0001 C CNN
F 1 "GND" H 6305 1727 50  0000 C CNN
F 2 "" H 6300 1900 50  0001 C CNN
F 3 "" H 6300 1900 50  0001 C CNN
	1    6300 1900
	1    0    0    -1  
$EndComp
$Comp
L Connector:Screw_Terminal_01x04 J2
U 1 1 60A30233
P 10300 3350
F 0 "J2" H 10380 3342 50  0000 L CNN
F 1 "MOTOR A" H 10380 3251 50  0000 L CNN
F 2 "TerminalBlock_TE-Connectivity:TerminalBlock_TE_282834-4_1x04_P2.54mm_Horizontal" H 10300 3350 50  0001 C CNN
F 3 "~" H 10300 3350 50  0001 C CNN
	1    10300 3350
	1    0    0    -1  
$EndComp
$Comp
L Connector:Screw_Terminal_01x04 J3
U 1 1 60A310A8
P 10300 3950
F 0 "J3" H 10380 3942 50  0000 L CNN
F 1 "MOTOR B" H 10380 3851 50  0000 L CNN
F 2 "TerminalBlock_TE-Connectivity:TerminalBlock_TE_282834-4_1x04_P2.54mm_Horizontal" H 10300 3950 50  0001 C CNN
F 3 "~" H 10300 3950 50  0001 C CNN
	1    10300 3950
	1    0    0    -1  
$EndComp
$Comp
L Regulator_Linear:L7805 U4
U 1 1 60749161
P 2950 900
F 0 "U4" H 2950 1142 50  0000 C CNN
F 1 "L7805" H 2950 1051 50  0000 C CNN
F 2 "Package_TO_SOT_THT:TO-220-3_Vertical" H 2975 750 50  0001 L CIN
F 3 "http://www.st.com/content/ccc/resource/technical/document/datasheet/41/4f/b3/b0/12/d4/47/88/CD00000444.pdf/files/CD00000444.pdf/jcr:content/translations/en.CD00000444.pdf" H 2950 850 50  0001 C CNN
	1    2950 900 
	1    0    0    -1  
$EndComp
Wire Wire Line
	10000 4900 10000 5000
Wire Wire Line
	3250 900  3350 900 
Wire Wire Line
	3350 900  3350 750 
Wire Wire Line
	2500 1950 2500 900 
Wire Wire Line
	2500 900  2650 900 
Connection ~ 2500 1950
Wire Wire Line
	2500 1950 2300 2000
Connection ~ 2300 2000
Wire Wire Line
	2950 1200 2950 1250
$Comp
L power:+5C #PWR0126
U 1 1 60766013
P 3350 750
F 0 "#PWR0126" H 3350 600 50  0001 C CNN
F 1 "+5C" H 3365 923 50  0000 C CNN
F 2 "" H 3350 750 50  0001 C CNN
F 3 "" H 3350 750 50  0001 C CNN
	1    3350 750 
	1    0    0    -1  
$EndComp
$Comp
L power:+5C #PWR0127
U 1 1 60779821
P 10000 5000
F 0 "#PWR0127" H 10000 4850 50  0001 C CNN
F 1 "+5C" H 10015 5173 50  0000 C CNN
F 2 "" H 10000 5000 50  0001 C CNN
F 3 "" H 10000 5000 50  0001 C CNN
	1    10000 5000
	-1   0    0    1   
$EndComp
$Comp
L Mechanical:MountingHole H1
U 1 1 607B75B2
P 1450 750
F 0 "H1" H 1550 796 50  0000 L CNN
F 1 "MountingHole" H 1550 705 50  0000 L CNN
F 2 "MountingHole:MountingHole_3.2mm_M3_DIN965_Pad" H 1450 750 50  0001 C CNN
F 3 "~" H 1450 750 50  0001 C CNN
	1    1450 750 
	1    0    0    -1  
$EndComp
$Comp
L Mechanical:MountingHole H2
U 1 1 607B798A
P 1450 950
F 0 "H2" H 1550 996 50  0000 L CNN
F 1 "MountingHole" H 1550 905 50  0000 L CNN
F 2 "MountingHole:MountingHole_3.2mm_M3_DIN965_Pad" H 1450 950 50  0001 C CNN
F 3 "~" H 1450 950 50  0001 C CNN
	1    1450 950 
	1    0    0    -1  
$EndComp
$Comp
L Mechanical:MountingHole H3
U 1 1 607B7C24
P 1450 1150
F 0 "H3" H 1550 1196 50  0000 L CNN
F 1 "MountingHole" H 1550 1105 50  0000 L CNN
F 2 "MountingHole:MountingHole_3.2mm_M3_DIN965_Pad" H 1450 1150 50  0001 C CNN
F 3 "~" H 1450 1150 50  0001 C CNN
	1    1450 1150
	1    0    0    -1  
$EndComp
$Comp
L Mechanical:MountingHole H4
U 1 1 607B7F30
P 1450 1350
F 0 "H4" H 1550 1396 50  0000 L CNN
F 1 "MountingHole" H 1550 1305 50  0000 L CNN
F 2 "MountingHole:MountingHole_3.2mm_M3_DIN965_Pad" H 1450 1350 50  0001 C CNN
F 3 "~" H 1450 1350 50  0001 C CNN
	1    1450 1350
	1    0    0    -1  
$EndComp
$Comp
L SamacSys_Parts:RAPC722BK J1
U 1 1 608CC470
P 1950 2050
F 0 "J1" H 2350 2315 50  0000 C CNN
F 1 "RAPC722BK" H 2350 2224 50  0000 C CNN
F 2 "RAPC722BK" H 2600 2150 50  0001 L CNN
F 3 "" H 2600 2050 50  0001 L CNN
F 4 "SWITCHCRAFT/CONXALL - RAPC722BK - CONNECTOR, DC POWER, SOCKET, 5A, THT" H 2600 1950 50  0001 L CNN "Description"
F 5 "11" H 2600 1850 50  0001 L CNN "Height"
F 6 "502-RAPC722BK" H 2600 1750 50  0001 L CNN "Mouser Part Number"
F 7 "https://www.mouser.co.uk/ProductDetail/Switchcraft/RAPC722BK?qs=Nl9mtr8tY%2Fsf3BOYJcSXfg%3D%3D" H 2600 1650 50  0001 L CNN "Mouser Price/Stock"
F 8 "Switchcraft" H 2600 1550 50  0001 L CNN "Manufacturer_Name"
F 9 "RAPC722BK" H 2600 1450 50  0001 L CNN "Manufacturer_Part_Number"
	1    1950 2050
	-1   0    0    -1  
$EndComp
Wire Wire Line
	1950 2050 1950 2000
Wire Wire Line
	1950 2000 2300 2000
Wire Wire Line
	1950 2200 1950 2150
Wire Wire Line
	1950 2200 2300 2200
$Comp
L Connector:Screw_Terminal_01x03 J4
U 1 1 608E7FF3
P 10300 4800
F 0 "J4" H 10250 5000 50  0000 L CNN
F 1 "12V/GND/5V" V 10400 4550 50  0000 L CNN
F 2 "TerminalBlock_TE-Connectivity:TerminalBlock_TE_282834-3_1x03_P2.54mm_Horizontal" H 10300 4800 50  0001 C CNN
F 3 "~" H 10300 4800 50  0001 C CNN
	1    10300 4800
	1    0    0    -1  
$EndComp
Wire Wire Line
	10100 4800 9800 4800
Wire Wire Line
	9800 4800 9800 5000
$Comp
L power:GND #PWR0128
U 1 1 608F307E
P 9800 5000
F 0 "#PWR0128" H 9800 4750 50  0001 C CNN
F 1 "GND" H 9805 4827 50  0000 C CNN
F 2 "" H 9800 5000 50  0001 C CNN
F 3 "" H 9800 5000 50  0001 C CNN
	1    9800 5000
	1    0    0    -1  
$EndComp
$Comp
L Device:C_Polarized_Small_US C6
U 1 1 608FA86D
P 3350 1050
F 0 "C6" H 3441 1096 50  0000 L CNN
F 1 "100uF" H 3441 1005 50  0000 L CNN
F 2 "Capacitor_THT:CP_Radial_D5.0mm_P2.50mm" H 3350 1050 50  0001 C CNN
F 3 "~" H 3350 1050 50  0001 C CNN
	1    3350 1050
	1    0    0    -1  
$EndComp
Wire Wire Line
	3350 950  3350 900 
Connection ~ 3350 900 
Wire Wire Line
	3350 1150 3350 1250
$Comp
L power:GND #PWR0125
U 1 1 6090A0F9
P 2950 1300
F 0 "#PWR0125" H 2950 1050 50  0001 C CNN
F 1 "GND" H 2955 1127 50  0000 C CNN
F 2 "" H 2950 1300 50  0001 C CNN
F 3 "" H 2950 1300 50  0001 C CNN
	1    2950 1300
	1    0    0    -1  
$EndComp
Wire Wire Line
	3350 1250 2950 1250
Connection ~ 2950 1250
Wire Wire Line
	2950 1250 2950 1300
$EndSCHEMATC
