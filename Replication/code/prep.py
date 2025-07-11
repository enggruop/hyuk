#pip install pandas
#pip install openpyxl
import pandas as pd
import os

## Set the working directory to the ‘Replication’ folder
os.chdir(r"C:\Users\ahnhy\Desktop\석사논문\Replication")


#파일 불러오기 
weo = pd.read_csv(r"data\raw_data\dataset_2025-05-24T07_57_18.566782687Z_DEFAULT_INTEGRATION_IMF.RES_WEO_6.0.0.csv")
interest = pd.read_csv(r"data\raw_data\interestrate_1990_2024.csv")
age = pd.read_excel(r"data\raw_data\WPP2024_POP_F02_1_POPULATION_5-YEAR_AGE_GROUPS_BOTH_SEXES.xlsx", sheet_name='Estimates', header=16)
agepredict = pd.read_excel(r"data\raw_data\WPP2024_POP_F02_1_POPULATION_5-YEAR_AGE_GROUPS_BOTH_SEXES.xlsx", sheet_name='Medium variant', header=16)
immigrant = pd.read_excel(r"data\raw_data\undesa_pd_2020_ims_stock_by_age_sex_and_destination.xlsx", sheet_name='Table 1', header=10, usecols='B,E,G:V')


alpha3_to_num = {
    "KOR": 410, "JPN": 392, "ISR": 376,
    "TUR": 792, "AUS": 36,  "NZL": 554, "USA": 840, "CAN": 124,
    "GBR": 826, "FRA": 250, "DEU": 276, "ITA": 380, "CHE": 756,
    "ESP": 724, "PRT": 620, "NLD": 528, "GRC": 300, "DNK": 208,
    "FIN": 246, "SWE": 752, "NOR": 578, "AUT": 40,  "IRL": 372,
    "ISL": 352, "MEX": 484, "CHL": 152, "POL": 616, "BEL": 56
}

# 1-2) Column Selection
cols = ['Location code','Year'] + [
    '0-4','5-9','10-14','15-19','20-24','25-29','30-34','35-39','40-44',
    '45-49','50-54','55-59','60-64','65-69','70-74','75-79','80-84',
    '85-89','90-94','95-99','100+'
]
age = age[cols].copy() # .copy()를 사용하여 SettingWithCopyWarning 방지
agepredict = agepredict[cols].copy() # .copy()를 사용하여 SettingWithCopyWarning 방지

# 1-3) Country filter and mapping (age 데이터프레임)
age = age[age['Location code'].isin(alpha3_to_num.values())]
num_to_alpha3 = {v:k for k,v in alpha3_to_num.items()}
age['Country.ID'] = age['Location code'].map(num_to_alpha3)
age.drop(columns=['Location code'], inplace=True)

# 1-4) column aggregate (age 데이터프레임)
age['0-19']  = age[['0-4','5-9','10-14','15-19']].sum(axis=1)
age['20-59'] = age[['20-24','25-29','30-34','35-39',
                  '40-44','45-49','50-54','55-59']].sum(axis=1)
age['60+']   = age[['60-64','65-69','70-74','75-79','80-84',
                  '85-89','90-94','95-99','100+']].sum(axis=1)

# 1-3) Country filter and mapping (agepredict 데이터프레임)
agepredict = agepredict[agepredict['Location code'].isin(alpha3_to_num.values())]
agepredict['Country.ID'] = agepredict['Location code'].map(num_to_alpha3)
agepredict.drop(columns=['Location code'], inplace=True)

# 1-4) column aggregate (agepredict 데이터프레임)
agepredict['0-19']  = agepredict[['0-4','5-9','10-14','15-19']].sum(axis=1)
agepredict['20-59'] = agepredict[['20-24','25-29','30-34','35-39',
                  '40-44','45-49','50-54','55-59']].sum(axis=1)
agepredict['60+']   = agepredict[['60-64','65-69','70-74','75-79','80-84',
                  '85-89','90-94','95-99','100+']].sum(axis=1)

# 두 데이터프레임을 합치기 전에 필요한 컬럼만 선택
cols_to_combine = ['Country.ID', 'Year', '0-19', '20-59', '60+']
combined_age_data = pd.concat([age[cols_to_combine], agepredict[cols_to_combine]], ignore_index=True)

# Country.ID와 Year로 정렬하여 popgrowth 계산 준비
combined_age_data = combined_age_data.sort_values(['Country.ID', 'Year'])

# total_pop 및 popgrowth 계산 (합쳐진 데이터프레임에서)
combined_age_data['total_pop'] = combined_age_data[['0-19', '20-59', '60+']].sum(axis=1)
combined_age_data['popgrowth'] = combined_age_data.groupby('Country.ID')['total_pop'].pct_change() * 100
combined_age_data.drop(columns=['total_pop'], inplace=True) # total_pop은 popgrowth 계산 후 삭제


# melt할 지표들을 리스트로 정의
# 'popgrowth'를 포함하여 모든 지표를 한 번에 처리
indicators_to_melt = ['0-19','20-59','60+', 'popgrowth']

# 합쳐진 데이터프레임을 사용하여 wide format 생성
age_melt = combined_age_data.melt(
    id_vars=['Country.ID','Year'],
    value_vars=indicators_to_melt, # 정의된 지표 리스트 사용
    var_name='INDICATOR',
    value_name='Value' # 'Population_thousands' 대신 'Value'로 변경하여 다양한 지표 값 포함
)
age_wide = age_melt.pivot(
    index=['Country.ID','INDICATOR'],
    columns='Year',
    values='Value' # 'Value' 컬럼 사용
).reset_index()
age_wide.columns.name = None

# 인구 관련 지표(0-19, 20-59, 60+)에만 1000을 곱하고, popgrowth는 그대로 유지
population_categories = ['0-19','20-59','60+']
for category in population_categories:
    # 해당 INDICATOR가 age_wide에 존재하는지 확인 후 곱셈 적용
    if category in age_wide['INDICATOR'].unique():
        # 해당 카테고리에 해당하는 행을 찾아 연도별 데이터에 1000 곱하기
        # .loc를 사용하여 안전하게 값 변경
        age_wide.loc[age_wide['INDICATOR'] == category, age_wide.columns[2:]] *= 1000

def is_year(col):
    try:
        year = int(float(col))
        return 1990 <= year <= 2024
    except:
        return False
year_cols = [col for col in age_wide.columns if is_year(col)]
age_wide = age_wide[['Country.ID', 'INDICATOR'] + year_cols]



####2. cpi, saving rate, gdp_p, unem prep
codes = [
    "KOR","JPN","ISR","TUR","AUS","NZL","USA","CAN",
    "GBR","FRA","DEU","ITA","CHE","ESP","PRT","NLD","GRC","DNK",
    "FIN","SWE","NOR","AUT","IRL","ISL","MEX","CHL","BEL","POL"
]
# 2-1) 지표 필터링 및 약어 매핑
indicator_map = {
    "Gross national savings, Percent of GDP":                                  "savingrate",
    "Gross domestic product (GDP), Constant prices, Percent change": "gdp_p",
    "All Items, Consumer price index (CPI), Period average, percent change":   "cpi",
    "Unemployment rate":                                                       "unem",
    "Gross capital formation, Percent of GDP" : "inv"
}
weo = weo[
    weo["COUNTRY.ID"].isin(codes) &
    weo["INDICATOR"].isin(indicator_map.keys())
].copy()
weo["INDICATOR"] = weo["INDICATOR"].map(indicator_map)

# 1-2) 연도 컬럼(1989–2024)만 남기기
years_all = [str(y) for y in range(1989, 2025)]
weo = weo[["COUNTRY.ID", "INDICATOR"] + years_all]



# 1-4) 최종 연도 컬럼(1990–2024)만 선택
years_final = [str(y) for y in range(1990, 2025)]
weo_final = weo[["COUNTRY.ID", "INDICATOR"] + years_final]
weo_final.rename(columns={"COUNTRY.ID": "Country.ID"}, inplace=True)




####3. Migrant code
# ——— 수정된 매핑 (Alpha‑3 → ISO numeric) ———
alpha3_to_num = {
    "KOR": 410, "JPN": 392, "ISR": 376,
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
group_young = ['0-4', '5-9', '10-14','15-19']
group_mid   = ['20-24', '25-29', '30-34', '35-39', 
               '40-44', '45-49', '50-54', '55-59']
group_old   = ['60-64','65-69', '70-74', '75+']

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





df_dict = {
    "age":               age_wide,
    "cpi_unem_s_gdp":    weo_final,
    "interestrate":      interest,
    "immigrant_final":   immigrant_transposed
}

long_frames = []
for name, df in df_dict.items():
    # (a) 복사본 생성
    tmp = df.copy()
    # (b) 컬럼 이름 통일: 'Country.ID', 'INDICATOR' 로 맞추기
    #    예: interest 데이터프레임이 'Indicator' 혹은 'INDICATOR' 중 하나일 수 있으니
    if 'Indicator' in tmp.columns:
        tmp = tmp.rename(columns={'Indicator': 'INDICATOR'})
    # (c) melt
    df_long = tmp.melt(
        id_vars=['Country.ID', 'INDICATOR'],
        var_name='Year',
        value_name='Value'
    )
    # (d) Year를 정수형으로
    df_long['Year'] = df_long['Year'].astype(int)
    long_frames.append(df_long)

# 2) 프레임 합치기
all_long = pd.concat(long_frames, ignore_index=True)

# 3) pivot_table로 패널 형태 생성
panel = all_long.pivot_table(
    index=['Country.ID', 'Year'],
    columns='INDICATOR',
    values='Value',
    aggfunc='first'
).reset_index()

# 5) 컬럼명 정리
panel.columns.name = None





panel['total'] = panel['0-19'] + panel['20-59'] + panel['60+']

panel['young'] = panel['0-19']   - panel['im_young']
panel['mid']   = panel['20-59']  - panel['im_mid']
panel['old']   = panel['60+']    - panel['im_old']
panel['rr']   = panel['interestrate']    - panel['cpi']
panel['imm'] = panel[['im_young', 'im_mid', 'im_old']].sum(axis=1)

panel['r_young'] = (panel['young'] / panel['total']) * 100
panel['r_mid']   = (panel['mid']   / panel['total']) * 100
panel['r_old']   = (panel['old']   / panel['total']) * 100
panel['r_imm']   = (panel['imm']   / panel['total']) * 100
panel=panel[["Country.ID","Year","gdp_p","cpi","interestrate","unem","savingrate","r_young","r_mid","r_old","r_imm","rr","inv","popgrowth"]]


df_oil = pd.read_csv(r"data\raw_data\DCOILWTICO.csv")

# 열 이름 통일: 날짜 → Year, 가격 → logoil
panel_oil = df_oil.rename(columns={
    'observation_date': 'Year',
    'DCOILWTICO':        'logoil'
})

# Year 열을 연도(int)로 변환
panel_oil['Year'] = pd.to_datetime(panel_oil['Year'], errors='coerce').dt.year

# 연도별 중복 제거 (최초 관측값 유지)
panel_oil = panel_oil[['Year', 'logoil']].drop_duplicates(subset='Year')

# 2) 병합: 기존 df에 logoil 열 추가
panel = panel.merge(panel_oil, on='Year', how='left')

# 3) 결과 저장
panel.to_csv(r"data\cleaned_data\panel.csv", index=False, encoding='utf-8-sig')