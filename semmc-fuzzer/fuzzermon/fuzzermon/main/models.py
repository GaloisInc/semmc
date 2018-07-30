from django.db import models
import signal

class User(models.Model):
    username = models.CharField(max_length=64, unique=True)

class Arch(models.Model):
    name = models.CharField(max_length=64, unique=True)

class Host(models.Model):
    hostname = models.CharField(max_length=128, unique=True)
    arch = models.ForeignKey(Arch, on_delete=models.CASCADE)

class Batch(models.Model):
    fuzzer_host = models.CharField(max_length=128)
    testing_host = models.ForeignKey(Host, on_delete=models.CASCADE, db_index=True)
    user = models.ForeignKey(User, on_delete=models.CASCADE)
    submitted_at = models.DateTimeField(auto_now_add=True, db_index=True)

class Opcode(models.Model):
    name = models.CharField(max_length=128, db_index=True)
    arch = models.ForeignKey(Arch, on_delete=models.CASCADE, db_index=True)

class Test(models.Model):
    batch = models.ForeignKey(Batch, on_delete=models.CASCADE, db_index=True)
    opcode = models.ForeignKey(Opcode, on_delete=models.CASCADE, db_index=True)
    pretty = models.CharField(max_length=256)
    bytes = models.CharField(max_length=256, default='')
    arguments = models.TextField()
    passed = models.BooleanField(default=False)
    signal = models.IntegerField(null=True)

    def signame(self):
        try:
            return signal.Signals(self.signal).name
        except ValueError:
            return "unknown"

class TestInput(models.Model):
    test = models.ForeignKey(Test, on_delete=models.CASCADE, db_index=True)
    location = models.CharField(max_length=128)
    value = models.CharField(max_length=256)

class TestSignalError(models.Model):
    batch = models.ForeignKey(Batch, on_delete=models.CASCADE, db_index=True)
    opcode = models.ForeignKey(Opcode, on_delete=models.CASCADE, db_index=True)
    pretty = models.CharField(max_length=256)
    bytes = models.CharField(max_length=256, default='')

class TestMachineState(models.Model):
    test = models.ForeignKey(Test, on_delete=models.CASCADE, db_index=True)
    location = models.CharField(max_length=128)
    expected_value = models.CharField(max_length=256)
    actual_value = models.CharField(max_length=256)
