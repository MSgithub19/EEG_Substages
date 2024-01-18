'''imports for spectrogram generation'''
import os.path
import numpy as np
from EEG import EEGIO, EEGvis
from Spectrogram_generator import Spectrogram

'''setting the filepath, make sure to actually include a file name at the end'''
file1= r"D:\Langston lab - in vivo electrophysiology\In Vivo Electrophysiology\Ephys FM JB OM\NN5-NN8\MRK\NN6\Drug 7\Free Moving\T4\NN6 FM D7 T4.egf"

#spec=Spectrogram(file1)
#spec.generate_spectrogram(min_freq=4.0, max_freq=8.0)

'''Generating a spectrogram and a sub spectrogram'''
spec_experiment = EEGIO.EEGexpt(file1)
spec = spec_experiment.channels[12].generate_spectrogram()
subspec1 = spec.getSubSpec((100,0), spec.tstart, spec.tend)
#print(subspec1.frequencies)

#plot = EEGvis.EEGplotter(subspec1)
#plot.plot_spectrogram()

'''extracting numpy matrix from the spectrogram and sub spectrogram'''
#matrix= spec.spec
sub_matrix= subspec1.spec

#this shows how many points occur in the time frame
#print(sub_matrix.shape)



'''Saving each spectrogram as a CSV file which is named after the time point or T that it was derived from, based on the filepath'''
directory_name= os.path.dirname(file1)
time_point= [t for t in directory_name.split(os.path.sep) if "T" in t][0]

#np.savetxt(f'{time_point}_matrix.csv', matrix, delimiter=',')

np.savetxt(f'{time_point}_C16.csv', sub_matrix, delimiter=',')






