from django.shortcuts import render
from django.http import HttpResponse, JsonResponse
from django.shortcuts import render, render_to_response
from django.template import RequestContext
from django.views.decorators.csrf import csrf_exempt
from .models import SpeciesYear
# from .views_helpers import geojson_creater

import json
# Create your views here.

import pandas as pd
from shapely.geometry import Point
import geopandas as gpd

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
    select2_species = "["
    for obj in SpeciesYear.objects.all():
        # lim = lim - 1
        # if (lim <= 0):
        #     break
        select2_species += obj.select2element()
        select2_species+=","
    select2_species += "]"
    return render(request, 'biodivmap/index.html', {'select2_species': select2_species})

@csrf_exempt
def ajax_species(request):
    if request.method == 'POST':
        if request.body:
            selected_taxons = json.loads(request.body)
            print(selected_taxons)

            df = pd.read_csv("biodivmap/GBif June27.csv", encoding="latin1")
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
            # selected_years = selected_species_years['years']
            # selected_species = selected_species_years['species']
            # species_path = []
            # for species in selected_species:
            #     species_dict = SpeciesYear.objects.filter(species=species['text']).values()[0]
            #     for year in selected_years:
            #         if species_dict["year_" + year['text']]:
            #             species_path.append(
            #                 {'path':species_dict["year_" + year['text']] + "points.geojson",
            #                  'species': species['text']})


    return JsonResponse(["yo"], safe=False)

def taxontree(request):
    return render(request, 'taxontree/index.html')
