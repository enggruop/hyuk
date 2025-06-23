#pip install pandas
#pip install openpyxl
import pandas as pd

#파일 불러오기 
weo=pd.read_csv("dataset_2025-05-24T07_57_18.566782687Z_DEFAULT_INTEGRATION_IMF.RES_WEO_6.0.0.csv")
interest=pd.read_csv("interestrate_1990_2024.csv")
age=pd.read_excel("WPP2024_POP_F02_1_POPULATION_5-YEAR_AGE_GROUPS_BOTH_SEXES.xlsx",sheet_name='Estimates', header=16)
immigrant=pd.read_excel("undesa_pd_2020_ims_stock_by_age_sex_and_destination.xlsx",sheet_name='Table 1', header=10, usecols='B,E,G:V')


#1. Age prep
# ——— Alpha‑3 → ISO numeric mapping ——
alpha3_to_num = {
    "KOR": 410, "JPN": 392, "SGP": 702, "ISR": 376,
    "TUR": 792, "AUS": 36,  "NZL": 554, "USA": 840, "CAN": 124,
    "GBR": 826, "FRA": 250, "DEU": 276, "ITA": 380, "CHE": 756,
    "ESP": 724, "PRT": 620, "NLD": 528, "GRC": 300, "DNK": 208,
    "FIN": 246, "SWE": 752, "NOR": 578, "AUT": 40,  "IRL": 372,
    "ISL": 352, "MEX": 484, "CHL": 152, "POL": 616, "BEL": 56
}

#1-2) Column Selection
cols = ['Location code','Year'] + [
    '0-4','5-9','10-14','15-19','20-24','25-29','30-34','35-39','40-44',
    '45-49','50-54','55-59','60-64','65-69','70-74','75-79','80-84',
    '85-89','90-94','95-99','100+'
]
age = age[cols]

#1-3) Country filter and mapping
age = age[age['Location code'].isin(alpha3_to_num.values())]
num_to_alpha3 = {v:k for k,v in alpha3_to_num.items()}
age['Country.ID'] = age['Location code'].map(num_to_alpha3)
age.drop(columns=['Location code'], inplace=True)

#1-4) column aggregate
age['0-14']  = age[['0-4','5-9','10-14']].sum(axis=1)
age['15-64'] = age[['15-19','20-24','25-29','30-34','35-39',
                  '40-44','45-49','50-54','55-59','60-64']].sum(axis=1)
age['65+']   = age[['65-69','70-74','75-79','80-84',
                  '85-89','90-94','95-99','100+']].sum(axis=1)
age_1 = age[['Country.ID','Year','0-14','15-64','65+']]

# 1-5) Wide format create
age_melt = age_1.melt(
    id_vars=['Country.ID','Year'],
    value_vars=['0-14','15-64','65+'],
    var_name='INDICATOR',
    value_name='Population_thousands'
)
age_wide = age_melt.pivot(
    index=['Country.ID','INDICATOR'],
    columns='Year',
    values='Population_thousands'
).reset_index()
age_wide.columns.name = None
age_wide.iloc[:, 2:] = age_wide.iloc[:, 2:] * 1000
##temporary before the release
age_wide[2024]=age_wide[2023]
def is_year(col):
    try:
        # 컬럼명을 float으로 해석한 뒤 int로 변환해 비교
        return int(float(col)) >= 1990
    except:
        return False
year_cols = [col for col in age_wide.columns if is_year(col)]
age_wide = age_wide[['Country.ID', 'INDICATOR'] + year_cols]
#1-6) save  
age_wide.to_csv('ageprep.csv', index=False, encoding='utf-8-sig')


####2. cpi, saving rate, gdp_p, unem prep
codes = [
    "KOR","JPN","SGP","ISR","TUR","AUS","NZL","USA","CAN",
    "GBR","FRA","DEU","ITA","CHE","ESP","PRT","NLD","GRC","DNK",
    "FIN","SWE","NOR","AUT","IRL","ISL","MEX","CHL","BEL","POL"
]
# 2-1) 지표 필터링 및 약어 매핑
indicator_map = {
    "Gross national savings, Percent of GDP":                                  "savingrate",
    "Gross domestic product (GDP), Constant prices, Per capita, Domestic currency": "gdp_p",
    "All Items, Consumer price index (CPI), Period average, percent change":   "cpi",
    "Unemployment rate":                                                       "unem"
}
weo = weo[
    weo["COUNTRY.ID"].isin(codes) &
    weo["INDICATOR"].isin(indicator_map.keys())
].copy()
weo["INDICATOR"] = weo["INDICATOR"].map(indicator_map)

# 1-2) 연도 컬럼(1989–2024)만 남기기
years_all = [str(y) for y in range(1989, 2025)]
weo = weo[["COUNTRY.ID", "INDICATOR"] + years_all]

# 1-3) gdp_p → 연평균 성장률(grgdp_p) 추가
weo_gdp = weo[weo["INDICATOR"] == "gdp_p"].copy()
weo_gdp[years_all] = weo_gdp[years_all].astype(float)
weo_growth = weo_gdp.copy()
weo_growth[years_all] = weo_growth[years_all].pct_change(axis=1) * 100
weo_growth["INDICATOR"] = "grgdp_p"

weo = pd.concat([weo[weo["INDICATOR"]!="gdp_p"], weo_growth], ignore_index=True)

# 1-4) 최종 연도 컬럼(1990–2024)만 선택
years_final = [str(y) for y in range(1990, 2025)]
weo_final = weo[["COUNTRY.ID", "INDICATOR"] + years_final]
weo_final.rename(columns={"COUNTRY.ID": "Country.ID"}, inplace=True)

# 2) 결과 저장
weo_final.to_csv("cpi_unem_s_grgdpp.csv", index=False, encoding="utf-8-sig")



####3. Migrant code
# ——— 수정된 매핑 (Alpha‑3 → ISO numeric) ———
alpha3_to_num = {
    "KOR": 410, "JPN": 392, "SGP": 702, "ISR": 376,
    "TUR": 792, "AUS": 36,  "NZL": 554, "USA": 840, "CAN": 124,
    "GBR": 826, "FRA": 250, "DEU": 276, "ITA": 380, "CHE": 756,
    "ESP": 724, "PRT": 620, "NLD": 528, "GRC": 300, "DNK": 208,
    "FIN": 246, "SWE": 752, "NOR": 578, "AUT": 40,  "IRL": 372,
    "ISL": 352, "MEX": 484, "CHL": 152, "POL":616, "BEL":56
}

# 숫자→Alpha‑3 뒤집기
num_to_alpha3 = {num: code for code, num in alpha3_to_num.items()}
immigrant = immigrant[immigrant['Location code'].isin(num_to_alpha3.keys())]
immigrant['Country.ID'] = immigrant['Location code'].map(num_to_alpha3)
immigrant = immigrant.drop(columns=['Location code'])
cols = ['Country.ID', 'Year'] + [c for c in immigrant.columns if c not in ['Country.ID', 'Year']]
group_young = ['0-4', '5-9', '10-14']
group_mid   = ['15-19', '20-24', '25-29', '30-34', '35-39', 
               '40-44', '45-49', '50-54', '55-59', '60-64']
group_old   = ['65-69', '70-74', '75+']

# Aggregate into new columns
immigrant['im_young'] = immigrant[group_young].sum(axis=1)
immigrant['im_mid']   = immigrant[group_mid].sum(axis=1)
immigrant['im_old']   = immigrant[group_old].sum(axis=1)
cols1 = ['Country.ID', 'Year', 'im_young', 'im_mid', 'im_old']
immigrant = immigrant[cols1]
# Linear interpolation from 1990 to 2024
years = list(range(1990, 2025))
idx = pd.MultiIndex.from_product(
    [immigrant['Country.ID'].unique(), years],
    names=['Country.ID','Year']
)
immigrant = immigrant.set_index(['Country.ID','Year']).reindex(idx)

# Convert to numeric (to avoid object‐dtype error)
immigrant[['im_young','im_mid','im_old']] = (
    immigrant[['im_young','im_mid','im_old']]
    .apply(pd.to_numeric, errors='coerce')
)

# Interpolate within each country
immigrant[['im_young','im_mid','im_old']] = (
    immigrant
    .groupby(level=0)[['im_young','im_mid','im_old']]
    .transform(lambda s: s.interpolate(method='linear'))
)

# Reset index and save
immigrant = immigrant.reset_index()
immigrant_long = immigrant.melt(
    id_vars=['Country.ID', 'Year'],
    value_vars=['im_young', 'im_mid', 'im_old'],
    var_name='INDICATOR',
    value_name='value'
)

# Pivot so that each Year becomes its own column
immigrant_transposed = immigrant_long.pivot_table(
    index=['Country.ID', 'INDICATOR'],
    columns='Year',
    values='value'
).reset_index()

# Remove the name of the columns index
immigrant_transposed.columns.name = None
immigrant_transposed.to_csv('immigrant_final.csv', index=False, encoding='utf-8-sig')





##integration  immigrant_transposed weo_final age_wide interest
files = [
    "ageprep.csv",
    "cpi_unem_s_grgdpp.csv",
    "interestrate_1990_2024.csv",
    "immigrant_final.csv"
]

# 2) long 포맷으로 읽어서 모으기
long_frames = []
for path in files:
    df = pd.read_csv(path, encoding='utf-8-sig')
    # melt: Country.ID, INDICATOR는 그대로, 연도별 컬럼을 Year/Value로 녹인다
    df_long = df.melt(
        id_vars=["Country.ID", "INDICATOR"],
        var_name="Year",
        value_name="Value"
    )
    # Year를 안전하게 정수로 변환 (예: "1990.0" → 1990)
    df_long["Year"] = pd.to_numeric(df_long["Year"], errors="coerce").dropna().astype(int)
    long_frames.append(df_long)

# 3) 하나로 합치기
all_long = pd.concat(long_frames, ignore_index=True)

# 4) pivot_table: Country.ID+Year 별로 INDICATOR를 열로 펼치기
panel = all_long.pivot_table(
    index=["Country.ID", "Year"],
    columns="INDICATOR",
    values="Value",
    aggfunc="first"   # 중복 시 첫 값을 사용
).reset_index()

# 5) 컬럼명 정리
panel.columns.name = None

# 6) CSV로 저장
panel.to_csv("merged_panel_1990_2024.csv", index=False, encoding="utf-8-sig")


df=pd.read_csv("merged_panel_1990_2024.csv")
df['total'] = df['0-14'] + df['15-64'] + df['65+']

df['young'] = df['0-14']   - df['im_young']
df['mid']   = df['15-64']  - df['im_mid']
df['old']   = df['65+']    - df['im_old']
df['imm'] = df[['im_young', 'im_mid', 'im_old']].sum(axis=1)

df['r_young'] = df['young'] / df['total']
df['r_mid']   = df['mid']   / df['total']
df['r_old'] = df['old'] / df['total']
df['r_imm']   = df['imm']   / df['total']
df=df[["Country.ID","Year","grgdp_p","cpi","interestrate","unem","savingrate","r_young","r_mid","r_old","r_imm"]]

df.to_csv("data.csv",index=False, encoding="utf-8-sig")