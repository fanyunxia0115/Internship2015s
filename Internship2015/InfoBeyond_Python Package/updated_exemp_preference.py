# calculate the preference of the current exemplar
import numpy as np 
def update_preference_exemp(cluster_centers_indices_current,affinity_matrix_current):
    preference_exemp=[]
    for i in range(len(cluster_centers_indices_current)):
        m=cluster_centers_indices_current[i]
        preference_exemp.append(affinity_matrix_current[m][m])
    preference_exemp=np.array(preference_exemp)
    return preference_exemp