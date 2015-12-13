import numpy as np
# used to initialize the first batch with the classic affinity propagation 
def init_ap(ap,x):
     cluster_centers_indices = ap.cluster_centers_indices_
     cluster_centers=ap.cluster_centers_
     labels=ap.labels_
     weighted_dissimilarity=[]
     exemplar_weight=[]
     affinity_matrix=ap.affinity_matrix_
     preference_exemp=[]
     #in the initialize step, we choose the median as the preference.
     for n in range(len(cluster_centers)):
           preference_exe=np.median(affinity_matrix)
           preference_exemp.append(preference_exe)
     #calcualte dissimilarity between points and exemplar
     for m in range(len(cluster_centers_indices)):
         s=0
         count=0
         for i in range(len(x)):
            if labels[i]==m:
                 s=s+sum((x[i]-cluster_centers[m])**2)
                 count=count+1
         weighted_dissimilarity.append(s)
         exemplar_weight.append(count)
     weighted_dissimilarity=np.array(weighted_dissimilarity)
     exemplar_weight=np.array(exemplar_weight)
     preference_exemp=np.array(preference_exemp)
     return cluster_centers,exemplar_weight,weighted_dissimilarity,preference_exemp
     