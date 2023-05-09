R= 0.1
L= 5.1e-06
C= 9.98e-05
V0= 5.14
I0= 1.1
alfa= 9803.921568627451
omega= 44325.09155329412

# Voltage:
V_A_fit= -5.332735563626728
V_omega= 43227.26922221391
V_phi= -1.84046945837198
V_decay= 9803.920301423423
 
#Current:
I_A_fit= 23.59012441030877
I_omega= 43227.26922221391
I_phi= -3.188239250193643
I_decay= 9803.92030142342


with open("series_parameters.dat", "a") as dc:
    dc.write(f"\n")
    dc.write(f"Errorv:\n")
    dc.write(f"decay= {alfa- V_decay}, {(alfa- V_decay)*100/alfa}\n")
    dc.write(f"omega= {omega-(V_omega*V_omega+V_decay*V_decay)**0.5}, {(omega-(V_omega*V_omega+V_decay*V_decay)**0.5)*100/omega}\n")
    dc.write(f"\n")
    dc.write(f"Errori:\n")
    dc.write(f"decay= {alfa- I_decay}, {(alfa- I_decay)*100/alfa}\n")
    dc.write(f"omega= {omega-(I_omega*I_omega+I_decay*I_decay)**0.5}, {(omega-(I_omega*I_omega+I_decay*I_decay)**0.5)*100/omega}\n")
    dc.write(f"\n")
