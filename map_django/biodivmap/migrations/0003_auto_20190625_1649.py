# Generated by Django 2.2.1 on 2019-06-25 16:49

from django.db import migrations, models


class Migration(migrations.Migration):

    dependencies = [
        ('biodivmap', '0002_auto_20190625_1646'),
    ]

    operations = [
        migrations.CreateModel(
            name='BiodivmapYear',
            fields=[
                ('species', models.CharField(max_length=100, primary_key=True, serialize=False)),
                ('new_column', models.IntegerField(blank=True, null=True)),
            ],
            options={
                'db_table': 'biodivmap_year',
                'managed': False,
            },
        ),
        migrations.DeleteModel(
            name='Year',
        ),
    ]
