# Generated by Django 2.2.1 on 2019-08-13 21:55

from django.db import migrations


class Migration(migrations.Migration):

    dependencies = [
        ('biodivmap', '0004_auto_20190813_2154'),
    ]

    operations = [
        migrations.RenameField(
            model_name='gbifsummary',
            old_name='redList',
            new_name='redlist',
        ),
    ]
