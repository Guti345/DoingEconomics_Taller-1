clear all
set more off

cd "C:\Users\anton\OneDrive\Documentos\GitHub\DoingEconomics_Taller-1\Scripts"

global main "C:\Users\anton\OneDrive\Documentos\GitHub\DoingEconomics_Taller-1"
global graphs "${main}\Outputs\Graphs"
global tables "${main}\Outputs\Tables"
global raw "${main}\Data\Raw"
global clean "${main}\Data\Clean"

use "${raw}\TenderosFU03_Publica.dta", clear // Upload de la base de datos.

keep Edad Educ Rol porqueNoDueno Capac_Interes falta_trabajadores Capac // Seleccionamos las Variables de interes.

save "${clean}\TenderosFU03_Publica_Capac.dta", replace

tab falta_trabajadores // Tabulamos opciones de respuesta
sum falta_trabajadores // Summary falta_trabajadores

tab Capac_Interes // Summary Capac_Interes
sum Capac_Interes // Tabulamos opciones de respuesta

tab Capac_Interes falta_trabajadores // Tabulación Cruzada


// Estadisticas Descriptivas
//ssc install estout, replace

tabstat falta_trabajadores, ///
	statistics(n mean p50 min max) ///
	columns(statistics)
	
estpost tabstat falta_trabajadores, ///
    statistics(n mean p50 min max) ///
    columns(statistics)

esttab using "${tables}\descriptivas_falta_trabajadores.tex", ///
    replace ///
    cells("n mean p50 min max") ///
    label ///
    nonumber ///
    title("Estadísticas descriptivas: Falta de trabajadores")

// Conteos (incluye missing)
tab Capac_Interes, matcell(F) missing

* Total
matrix T = J(rowsof(F),1,0)
scalar N = 0
forvalues i=1/`=rowsof(F)' {
scalar N = N + F[`i',1]
}

* Proporción (y %)
matrix P = F / N
matrix Pct = 100*P
matrix rownames Pct = "Sí" "No" "NA" // labels
matrix colnames Pct = "Porcentaje" // labels

* Exportar a LaTeX
esttab matrix(Pct) using "${tables}\proporcion_capac_interes.tex", ///
replace ///
title("Proporción de interés en capacitación") ///
nonumber ///
nomtitles ///
noobs ///
cells("Pct(fmt(2))")

// Gráficas
count if inrange(falta_trabajadores, 1, 5) // Cuenta de Frecuencias Tabla
local N = r(N) // N count (muestra)
count if missing(falta_trabajadores)
local na = r(N)

// Definición de labels por categoria
label define likert_lbl ///
    1 "Muy perjudicial" ///
    2 "Perjudicial" ///
    3 "Neutral" ///
    4 "Poco perjudicial" ///
    5 "Nada perjudicial"

label values falta_trabajadores likert_lbl // label categorias con variable

// Gráfico de barras trabajadores 
graph bar (percent), /// 
    over(falta_trabajadores) ///
    blabel(bar, format(%4.1f) position(outside)) ///
    ytitle("Porcentaje (%)") ///
    title("Percepción Falta de Tenderos Capacitados") ///
    subtitle("N = `N' ; NA = `na'") ///
    bargap(10)
	
graph export "${graphs}\falta_trabajadores.png", replace

count if inrange(Capac_Interes, 1, 2) // Cuenta de Frecuencias Tabla
local N = r(N) // N count (muestra)
count if missing(Capac_Interes)
local na = r(N)

// Definición de labels por categoria
label define preg2 ///
    1 "Si" ///
    2 "No" 

label values Capac_Interes preg2 // label categorias con variable

// Gráfico de barras interés capacitación
graph bar (percent), /// 
    over(Capac_Interes) ///
    blabel(bar, format(%4.1f) position(outside)) ///
    ytitle("Porcentaje (%)") ///
    title("Interés sobre la capacitación de trabajadores") ///
    subtitle("Capac(No) = `N' ; Capac(Sí) = `na'") ///
    bargap(10) ///
    bar(1, col(green)) 
	
graph export "${graphs}\Capac_Interes.png", replace // gráfico de barras

// ssc install heatplot, replace
// ssc install palettes, replace
// ssc install colrspace, replace

// 1) Crear categorías NA
gen CI_hm = Capac_Interes
replace CI_hm = 99 if missing(Capac_Interes)

gen FT_hm = falta_trabajadores
replace FT_hm = 99 if missing(falta_trabajadores)

// Labels para heatmap (incluye NA)
label define CI_hm_lbl 1 "Sí" 2 "No" 99 "NA", replace
label values CI_hm CI_hm_lbl

label define FT_hm_lbl ///
    1 "Muy perjudicial" ///
    2 "Perjudicial" ///
    3 "Neutral" ///
    4 "Poco perjudicial" ///
    5 "Nada perjudicial" ///
    99 "NA", replace
label values FT_hm FT_hm_lbl

// 2) Total de observaciones (incluye missing)
count
local N = r(N)

// 3) Tabla cruzada incluyendo NA como categoría
tab CI_hm FT_hm, matcell(M)

// 4) Convertir a % del total
mata:
M = st_matrix("M")
ts = strtoreal(st_local("N"))
P = (M :/ ts) * 100
st_matrix("P", P)
end


// 5) Heatmap con labels reales en ejes
heatplot P, ///
    colors(reds) ///
    title("Heat map: Falta de trabajadores vs Interés en capacitación") ///
    subtitle("Porcentaje sobre el total de observaciones (incluye NA). N = `N'") ///
    xtitle("Falta de trabajadores capacitados") ///
    ytitle("Interés en capacitación") ///
    xlabel( ///
        1 "Muy perjudicial" ///
        2 "Perjudicial" ///
        3 "Neutral" ///
        4 "Poco perjudicial" ///
        5 "Nada perjudicial" ///
        6 "NA", ///
        angle(30)) ///
    ylabel( ///
        1 "Sí" ///
        2 "No" ///
        3 "NA", ///
        angle(0))

graph export "${graphs}\heatmap_twoq.png", replace // exportar heatmap

