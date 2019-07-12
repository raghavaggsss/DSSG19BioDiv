from django.shortcuts import render
from django.http import HttpResponse, JsonResponse
from django.shortcuts import render, render_to_response
from django.template import RequestContext
from django.views.decorators.csrf import csrf_exempt
from .models import SpeciesYear
# from .views_helpers import geojson_creater

import json
import numpy as np
# Create your views here.

import pandas as pd
from shapely.geometry import Point
import geopandas as gpd

taxLevel = ['kingdom', 'phylum', 'class', 'order', 'family', 'genus', 'species', 'end']
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
            selected_taxons = json.loads(request.body)
            print(selected_taxons)

            df = pd.read_csv("biodivmap/gbif_map.csv", encoding="latin1")
            df = df.drop(['Unnamed: 0', 'Winter', 'Spring', 'Summer', 'Fall'], 1)
            df_out = pd.DataFrame(columns=df.columns)
            for taxon_name, info_dict in selected_taxons.items():
                df_out = df_out.merge(df[df[info_dict["taxLevel"]] == taxon_name], how="outer")
            geometry = [Point(xy) for xy in zip(df_out['decimalLongitude'], df_out['decimalLatitude'])]
            # fix coordinate system
            geo_df = gpd.GeoDataFrame(df_out, geometry=geometry, crs={'init': 'epsg:4326'})
            geo_df = geo_df.drop(["decimalLongitude", "decimalLatitude", 'kingdom', 'phylum', 'class',
                        'order', 'family', 'genus'], 1)
            geo_df.to_file("biodivmap/static/biodivmap/curr.geojson", driver="GeoJSON")

    return JsonResponse(["yo"], safe=False)

@csrf_exempt
def show_summary(request):
    if request.method == 'POST':
        if request.body:
            selected_regions = json.loads(request.body)
            print(selected_regions)
            # assume municipality json created as bar_sunburst.json
            #filter dataframe with municipality
            df_obs = pd.read_csv("biodivmap/gbif_summary.csv", encoding="latin1")
            df_obs_mun = df_obs[df_obs['municipality'] == selected_regions["municipality"]]
            df_taxon = pd.read_csv("biodivmap/Taxonomy Freq.csv", encoding="latin1")
            df_taxon_mun = df_taxon[df_taxon["species"].isin(df_obs_mun['species'])]
            df_taxon_mun = df_taxon_mun.set_index("species")
            # species_gp = df_obs_mun.groupby(["species"])
            # for spec, gp_spec in species_gp:
            #     df_taxon_mun.loc[df_taxon_mun.loc[df_taxon_mun['species'] == spec].index, "freq"] = len(gp_spec)
            # print(df_taxon_mun.shape)
            df_freq = df_obs_mun["species"].value_counts().to_frame()
            df_freq.columns = ["mun_freq"]
            df = pd.merge(df_taxon_mun, df_freq, left_index=True, right_index=True)

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
