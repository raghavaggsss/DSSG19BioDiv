from django.shortcuts import render
from django.http import HttpResponse, JsonResponse
from django.shortcuts import render, render_to_response
from django.template import RequestContext
from django.views.decorators.csrf import csrf_exempt
from .models import SpeciesYear
# from .views_helpers import geojson_creater

import json
import pandas as pd
from shapely.geometry import Point
import geopandas as gpd

taxLevel = ['kingdom', 'phylum', 'class', 'order', 'family', 'genus', 'species', 'end']
df_map = pd.read_pickle("biodivmap/gbif_map.pkl")
df_map = df_map.drop(['Unnamed: 0', 'Winter', 'Spring', 'Summer', 'Fall'], 1)
df_obs = pd.read_pickle("biodivmap/gbif_summary.pkl")
df_taxon = pd.read_csv("biodivmap/Taxonomy Freq.csv", encoding="latin1")

def getDict(df, taxLevelIndex, prevIndex):
    list_dicts = []
    if taxLevel[taxLevelIndex] == 'end':
        return ([], 0)
    if taxLevel[taxLevelIndex] == 'kingdom':
        df_size = df.shape[0]
        gp = df.groupby(taxLevel[taxLevelIndex])

    else:
        df_size = df[df[taxLevel[taxLevelIndex - 1]] == prevIndex].shape[0]
        gp = df[df[taxLevel[taxLevelIndex - 1]] == prevIndex].groupby(taxLevel[taxLevelIndex])

    indexes = gp.size().index.to_list()
    values = list(gp.size().values)
    num_types = len(gp)
    ind_val = zip(indexes, values)
    ind_val = sorted(ind_val, key=lambda tup: tup[1], reverse=True)
    for i in ind_val:
        index = i[0]
        value = i[1]
        next_data, num_types_next = getDict(df, taxLevelIndex + 1, index)
        curr_gp = gp.get_group(index)
        redList = 0
        if (curr_gp.redList.unique().shape[0] > 1 or not pd.isna(curr_gp.redList.unique()[0])):
            redList = 1
        if (taxLevel[taxLevelIndex] == "species"):

            list_dicts.append({"name": index,
                               "value": int(curr_gp.iloc[0].mun_freq),
                               "size": int(value), "children": next_data,
                               })
        else:
            list_dicts.append({"name": index,

                               "children": next_data,
                               })

    return (list_dicts, num_types)


def index(request):
    # if request.method == 'POST':
    #     form = SpeciesForm(request.POST)
    #     if form.is_valid():
    #         species = form.cleaned_data.get('species')
    #         # do something with your results
    #         print(species)
    #         for spec in species:
    #             print(spec)
    # else:
    #     form = SpeciesForm

    # form = SpeciesForm

    # select2_species = "[{id: 'rag', text: 'rag'}, {id: 'raggie', text: 'raggie'}]"
    # lim = 1000
    # select2_species = "["
    # for obj in SpeciesYear.objects.all():
        # # lim = lim - 1
        # # if (lim <= 0):
        # #     break
        # select2_species += obj.select2element()
        # select2_species+=","
    # select2_species += "]"
    # return render(request, 'biodivmap/index.html', {'select2_species': select2_species})
    return render(request, 'biodivmap/index.html')

@csrf_exempt
def ajax_species(request):
    if request.method == 'POST':
        if request.body:
            taxons_regions = json.loads(request.body)
            print(taxons_regions)
            if not bool(taxons_regions["taxons"]):
                return JsonResponse("no selection", safe=False)
            bbox = taxons_regions["bbox"]
            min_x = min([bbox[0], bbox[2]])
            max_x = max([bbox[0], bbox[2]])
            min_y = min([bbox[1], bbox[3]])
            max_y = max([bbox[1], bbox[3]])
            df_region = df_map[(df_map["decimalLatitude"] > min_y) & (df_map["decimalLongitude"] > min_x) &
                                       (df_map["decimalLatitude"] < max_y) & (df_map["decimalLongitude"] < max_x)]
            df_out = pd.DataFrame(columns=df_region.columns)
            for taxon_name, info_dict in taxons_regions["taxons"].items():
                if df_region[df_region[info_dict["taxLevel"]] == taxon_name].shape[0] > 0:
                    df_out = df_out.merge(df_region[df_region[info_dict["taxLevel"]] == taxon_name], how="outer")

            geometry = [Point(xy) for xy in zip(df_out['decimalLongitude'], df_out['decimalLatitude'])]
            # fix coordinate system
            geo_df = gpd.GeoDataFrame(df_out, geometry=geometry, crs={'init': 'epsg:4326'})
            geo_df = geo_df.drop(["decimalLongitude", "decimalLatitude", 'kingdom', 'phylum', 'class',
                        'order', 'family', 'genus'], 1)
            if geo_df.shape[0] > 0:
                geo_df.to_file("biodivmap/static/biodivmap/curr.geojson", driver="GeoJSON")
                return JsonResponse("success", safe=False)
            else:
                return JsonResponse("no occurrence", safe=False)

    return JsonResponse("success", safe=False)

@csrf_exempt
def show_summary(request):
    if request.method == 'POST':
        if request.body:
            selected_regions = json.loads(request.body)
            # assume municipality json created as bar_sunburst.json
            #filter dataframe with municipality

            if "municipality" in selected_regions.keys():
                df_obs_region = df_obs[df_obs['municipality'] == selected_regions["municipality"]]

            else:
                bbox = selected_regions["bbox"]
                min_x = min([bbox[0], bbox[2]])
                max_x = max([bbox[0], bbox[2]])
                min_y = min([bbox[1], bbox[3]])
                max_y = max([bbox[1], bbox[3]])
                df_obs_region = df_obs[(df_obs["decimalLatitude"] > min_y) & (df_obs["decimalLongitude"] > min_x) &
                                       (df_obs["decimalLatitude"] < max_y) & (df_obs["decimalLongitude"] < max_x)]

                # bbox_poly = Polygon([(min_x, min_y), (max_x, min_y), (max_x, max_y), (min_x, max_y)])
                # geometry = [Point(xy) for xy in zip(df_obs['decimalLongitude'], df_obs['decimalLatitude'])]
                # geo_df = gpd.GeoDataFrame(df_obs, geometry=geometry, crs={'init': 'epsg:4326'})
                # df_obs_region = geo_df[geo_df.geometry.within(bbox_poly)]

            df_taxon_region = df_taxon[df_taxon["species"].isin(df_obs_region['species'])]
            df_taxon_region = df_taxon_region.set_index("species")
            # species_gp = df_obs_mun.groupby(["species"])
            # for spec, gp_spec in species_gp:
            #     df_taxon_mun.loc[df_taxon_mun.loc[df_taxon_mun['species'] == spec].index, "freq"] = len(gp_spec)
            # print(df_taxon_mun.shape)
            df_freq = df_obs_region["species"].value_counts().to_frame()
            df_freq.columns = ["mun_freq"]
            df = pd.merge(df_taxon_region, df_freq, left_index=True, right_index=True)

            # create jsons for sunburst and bar chart
            df["species"] = df.index
            df[['kingdom', 'phylum', 'class',
                'order', 'family', 'genus', 'species']] = df[['kingdom',
                                                              'phylum', 'class', 'order', 'family',
                                                   'genus', 'species']].fillna(value="Unknown")
            json_dict, num_types = getDict(df, 0, "blah")
            json_dict = {"name": "Organisms", "children": json_dict}

            with open('biodivmap/static/biodivmap/bar_sunburst.json', 'w') as fp:
                json.dump(json_dict, fp)

    return JsonResponse(["yo"], safe=False)
