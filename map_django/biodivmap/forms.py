from django import forms
from .models import SpeciesYear
from django_select2.forms import Select2MultipleWidget

# class SpeciesForm(forms.Form):
#
#     species = forms.ModelMultipleChoiceField(widget=Select2MultipleWidget,
#                                              queryset=SpeciesYear.objects.all())
#
