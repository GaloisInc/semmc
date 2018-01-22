"""fuzzermon URL Configuration

The `urlpatterns` list routes URLs to views. For more information please see:
    https://docs.djangoproject.com/en/2.0/topics/http/urls/
Examples:
Function views
    1. Add an import:  from my_app import views
    2. Add a URL to urlpatterns:  path('', views.home, name='home')
Class-based views
    1. Add an import:  from other_app.views import Home
    2. Add a URL to urlpatterns:  path('', Home.as_view(), name='home')
Including another URLconf
    1. Import the include() function: from django.urls import include, path
    2. Add a URL to urlpatterns:  path('blog/', include('blog.urls'))
"""
# from django.contrib import admin
from django.urls import path

from main import views

urlpatterns = [
    path('', views.arch_list),
    path('arch/<int:arch_id>/', views.view_arch),
    path('opcode/<int:opcode_id>/', views.view_opcode),
    path('test_failure/<int:test_failure_id>/', views.view_test_failure),
    path('test_signal_error/<int:test_signal_error_id>/', views.view_test_signal_error),
    path('upload_batch', views.upload_batch),
    # path('admin/', admin.site.urls),
]
