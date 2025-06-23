clear
cd "C:\Users\ahnhy\OneDrive\Desktop\RA\야구"
use gamelog2324.dta



***1. 전체승률
preserve

drop if missing(타석당스트라이크)

split 경기결과홈원정, parse(":") generate(score)
gen 홈팀승 = (real(score1) >= real(score2))
drop if (real(score1) == real(score2))

collapse (mean) 홈팀승, by(시즌 구장 날짜)
collapse (mean) 홈팀승, by(시즌)
gen 홈팀승_pct = 홈팀승 * 100


graph bar (asis) 홈팀승_pct if inlist(시즌, 2023, 2024), ///
    over(시즌) ///
    asyvars ///
    bar(1, bcolor(navy)) ///
    bar(2, bcolor(orange)) ///
    ytitle("홈팀승률(%)") ///
    title("2023 vs 2024 시즌 홈승률 비교") ///
    blabel(bar, format(%9.1f))

restore

***2. 전체 스트라이크 볼 비율
preserve
generate 스트라이크비율 = 타석당스트라이크 / 타석당투구수 if 타석당투구수 != 0
generate 볼비율 = 타석당볼 / 타석당투구수 if 타석당투구수 != 0

collapse (mean) 스트라이크비율 볼비율, by(시즌 투수홈여부)

generate 스트라이크비율_pct = 스트라이크비율 * 100
generate 볼비율_pct = 볼비율 * 100

*2-1) 스트라이크비율 백분율 막대그래프
graph bar (asis) 스트라이크비율_pct, ///
    over(투수홈여부) over(시즌) ///
    asyvars ///
    blabel(bar, format(%9.1f)) ///
    ytitle("스트라이크비율 (%)") ///
    title("시즌 및 홈어웨이별 스트라이크비율") ///
    bar(1, bcolor(navy)) bar(2, bcolor(orange))


*2-2) 볼비율 백분율 막대그래프
graph bar (asis) 볼비율_pct, ///
    over(투수홈여부) over(시즌) ///
    asyvars ///
    blabel(bar, format(%9.1f)) ///
    ytitle("볼비율 (%)") ///
    title("시즌 및 홈어웨이별 볼비율") ///
    bar(1, bcolor(navy)) bar(2, bcolor(orange))
restore



***3)ABS 제도 도입이 홈 경기에서의 타석당 볼 비율을 높이는가?

preserve
drop if missing(타석당스트라이크)
generate byte 투수홈여부bin = (투수홈여부 == "홈")
generate byte ABS여부 = (시즌 == 2024)
generate float 볼비율 = .
replace 볼비율 = 타석당볼 / 타석당투구수 if 타석당투구수 != 0
generate interaction = ABS여부 * 투수홈여부bin
regress 볼비율 ABS여부 투수홈여부bin interaction
restore

***3.1)팀별 및 상황별, "3루|2,3루|만루" 부분을 수정 

preserve
drop if missing(타석당스트라이크)
keep if regexm(이전상황, "2루|3루|1,3루|2,3루만루")

generate byte 투수홈여부bin = (투수홈여부 == "홈")
generate byte ABS여부 = (시즌 == 2024)

generate float 볼비율 = .
replace 볼비율 = 타석당볼 / 타석당투구수 if 타석당투구수 != 0

generate interaction = ABS여부 * 투수홈여부bin


statsby _b_interaction = _b[interaction] ///
       _se_interaction = _se[interaction], ///
    by(홈팀) nodots clear: ///
    regress 볼비율 ABS여부 투수홈여부bin interaction


gen lb = _b_interaction - 1.96 * _se_interaction
gen ub = _b_interaction + 1.96 * _se_interaction


encode 홈팀, gen(team_id)
levelsof team_id, local(teams)


levelsof 홈팀, local(teamlist)
local graphcmd ""

foreach t of local teamlist {

    local col "black"  // 기본값
    if "`t'" == "KIA"   local col "red"
    if "`t'" == "KT"    local col "black"
    if "`t'" == "LG"    local col "maroon"
    if "`t'" == "NC"    local col "navy"
    if "`t'" == "SSG"   local col "magenta"
    if "`t'" == "두산"  local col "blue"
    if "`t'" == "롯데"  local col "orange"
    if "`t'" == "삼성"  local col "skyblue"
    if "`t'" == "키움"  local col "brown"
    if "`t'" == "한화"  local col "yellow"
    

    local graphcmd `graphcmd' ///
        (rcap ub lb team_id if 홈팀=="`t'", ///
            lwidth(thick) lcolor("`col'")) ///
        (scatter _b_interaction team_id if 홈팀=="`t'", ///
            msymbol(circle) msize(medium) mcolor("`col'"))
}

twoway `graphcmd', ///
    xlabel(`teams', valuelabel) ///
    title("팀별 상호작용 항 계수 및 95% 신뢰구간") ///
    ytitle("Interaction 계수") ///
    xtitle("볼비율변화-득점권상황") ///
	legend(off)
	
restore

***4)ABS 제도 도입이 홈 경기에서의 타석당 스트라이크 비율을 낮추는가? 

preserve
drop if missing(타석당스트라이크)
*keep if regexm(이전상황, "2루|3루|1,3루|2,3루만루")
generate byte 투수홈여부bin = (투수홈여부 == "홈")
generate byte ABS여부 = (시즌 == 2024)
generate float 스트라이크비율 = .
replace 스트라이크비율 = 타석당스트라이크 / 타석당투구수 if 타석당투구수 != 0
generate interaction = ABS여부 * 투수홈여부bin
regress 스트라이크비율 ABS여부 투수홈여부bin interaction
sort 홈팀
by 홈팀: regress 스트라이크비율 ABS여부 투수홈여부bin interaction
restore


***4.1)팀별 및 상황별, "3루|2,3루|만루" 부분을 수정 

preserve
drop if missing(타석당스트라이크)
keep if regexm(이전상황, "2루|3루|1,3루|2,3루|만루|1,2루")

generate byte 투수홈여부bin = (투수홈여부 == "홈")
generate byte ABS여부 = (시즌 == 2024)

generate float 스트라이크비율 = .
replace 스트라이크비율 = 타석당스트라이크 / 타석당투구수 if 타석당투구수 != 0

generate interaction = ABS여부 * 투수홈여부bin


statsby _b_interaction = _b[interaction] ///
       _se_interaction = _se[interaction], ///
    by(홈팀) nodots clear: ///
    regress 스트라이크비율 ABS여부 투수홈여부bin interaction


gen lb = _b_interaction - 1.96 * _se_interaction
gen ub = _b_interaction + 1.96 * _se_interaction


encode 홈팀, gen(team_id)
levelsof team_id, local(teams)


levelsof 홈팀, local(teamlist)
local graphcmd ""

foreach t of local teamlist {

    local col "black"  
    if "`t'" == "KIA"   local col "red"
    if "`t'" == "KT"    local col "black"
    if "`t'" == "LG"    local col "maroon"
    if "`t'" == "NC"    local col "navy"
    if "`t'" == "SSG"   local col "magenta"
    if "`t'" == "두산"  local col "blue"
    if "`t'" == "롯데"  local col "orange"
    if "`t'" == "삼성"  local col "skyblue"
    if "`t'" == "키움"  local col "brown"
    if "`t'" == "한화"  local col "yellow"
    

    local graphcmd `graphcmd' ///
        (rcap ub lb team_id if 홈팀=="`t'", ///
            lwidth(thick) lcolor("`col'")) ///
        (scatter _b_interaction team_id if 홈팀=="`t'", ///
            msymbol(circle) msize(medium) mcolor("`col'"))
}

twoway `graphcmd', ///
    xlabel(`teams', valuelabel) ///
    title("팀별 상호작용 항 계수 및 95% 신뢰구간") ///
    ytitle("Interaction 계수") ///
    xtitle("스트라이크비율-득점권상황") ///
	legend(off)
	
restore


***5) 타석당 투구수별
preserve
tempfile orig
drop if missing(타석당스트라이크)
generate byte 투수홈여부bin = (투수홈여부 == "홈")
generate byte ABS여부 = (시즌 == 2024)
generate float 스트라이크비율 = .
replace 스트라이크비율 = 타석당스트라이크 / 타석당투구수 if 타석당투구수 != 0
generate interaction = ABS여부 * 투수홈여부bin
save `orig', replace

drop if missing(타석당스트라이크)
collapse (sum) 시즌투구수 = 타석당투구수 (count) 타석수 = 타석당투구수, by(투수)
generate 타석당평균투구수 = 시즌투구수 / 타석수
drop if 시즌투구수 < 150
xtile group = 타석당평균투구수, nq(4)
save "pitcher_summary.dta", replace
restore

preserve
use `orig', clear
merge m:1 투수 using "pitcher_summary.dta", keepusing(group) nogen

levelsof group, local(groups)
foreach g of local groups {
    di "---------------------------------------------"
    di "Regression for Group `g'"
    regress 스트라이크비율 ABS여부 투수홈여부bin interaction if group == `g'
}

tempname handle
tempfile results
postutil clear
postfile `handle' byte(group) double(interaction_coef se_interaction) using "results", replace

levelsof group, local(groups)
foreach g of local groups {
    di "---------------------------------------------"
    di "Regression for Group `g'"
    regress 스트라이크비율 ABS여부 투수홈여부bin interaction if group == `g'
    post `handle' (`g') (_b[interaction]) (_se[interaction])
}
postclose `handle'

use "results", clear

gen lb = interaction_coef - 1.96 * se_interaction
gen ub = interaction_coef + 1.96 * se_interaction

list, sep(0)

twoway (rcap ub lb group) (scatter interaction_coef group, msymbol(circle) mcolor(red)), ///
    xlabel(1 "1/4분위" 2 "2/4분위" 3 "3/4분위" 4 "4/4분위") ///
    title("타석당 투구수 그룹별 상호작용항") ///
    ytitle("Interaction항 계수") ///
    xtitle("1타석당 투구수")
	legend(off)
restore
