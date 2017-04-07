import os
import shutil

direct = '/Users/kirsten/Documents/data/RF_trajectories/Auto_bfast/'
os.chdir(direct)

fldrs = os.listdir('.')

for fldr in fldrs:
    if fldr.split('_')[0] == 'Bfast':
        os.chdir(direct + fldr)
        for fil in os.listdir('.'):
            if fil[-3:] == 'pdf':
                noSensor = fil.split('_')[-1].replace('.pdf','')
                newId = int(filter(str.isdigit, noSensor))
                if (str(newId)[-3:] == '131' and len(str(newId)) > 3):
                    newId = int(str(newId)[:-3])
                if (str(newId)[-3:] == '434' and len(str(newId)) > 3):
                    newId = int(str(newId)[:-3])

                strt = len(str(newId))

                sensor = noSensor[strt:]
                
                if not os.path.exists(direct+sensor):
                    os.makedirs(direct+sensor)

                shutil.copyfile(direct + fldr + '/' + fil, direct + sensor + '/Pt'+ str(newId) + '_'+fil)
                    
