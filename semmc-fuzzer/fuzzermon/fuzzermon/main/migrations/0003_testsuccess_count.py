# Generated by Django 2.0.1 on 2018-01-15 23:45

from django.db import migrations, models


class Migration(migrations.Migration):

    dependencies = [
        ('main', '0002_testfailurestate'),
    ]

    operations = [
        migrations.AddField(
            model_name='testsuccess',
            name='count',
            field=models.IntegerField(default=0),
            preserve_default=False,
        ),
    ]
