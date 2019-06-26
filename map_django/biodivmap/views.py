from django.shortcuts import render
from django.http import HttpResponse, JsonResponse
from django.shortcuts import render, render_to_response
from django.template import RequestContext
from django.views.decorators.csrf import csrf_exempt
from .models import SpeciesYear

import json
# Create your views here.

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
    select2_species = "["
    for obj in SpeciesYear.objects.all():
        select2_species += obj.select2element()
        select2_species+=","
    select2_species += "]"
    return render(request, 'biodivmap/index.html', {'select2_species': select2_species})

@csrf_exempt
def ajax_species(request):
    if request.method == 'POST':
        if request.body:
            selected_species = json.loads(request.body)
            for species in selected_species:
                print(species['text'])

    return JsonResponse({'foo': 'bar'})

