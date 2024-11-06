package com.protreino.services.usecase;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

import javax.swing.Icon;
import javax.swing.JButton;
import javax.swing.JOptionPane;
import javax.swing.SwingWorker;

import com.protreino.services.constants.Origens;
import com.protreino.services.constants.Tipo;
import com.protreino.services.devices.Device;
import com.protreino.services.devices.ServerDevice;
import com.protreino.services.entity.LogPedestrianAccessEntity;
import com.protreino.services.entity.PedestrianAccessEntity;
import com.protreino.services.enumeration.BroadcastMessageType;
import com.protreino.services.enumeration.Manufacturer;
import com.protreino.services.enumeration.NotificationType;
import com.protreino.services.enumeration.VerificationResult;
import com.protreino.services.main.Main;
import com.protreino.services.repository.HibernateAccessDataFacade;
import com.protreino.services.screens.AutenticationDialog;
import com.protreino.services.screens.ReleaseAccessDialog;
import com.protreino.services.screens.ReleaseReasonDialog;
import com.protreino.services.to.BroadcastMessageTO;
import com.protreino.services.utils.Utils;

public class ReleaseAccessUseCase {
	
	private static boolean apertouF9 = false;
    private static boolean apertouF10 = false;

	private final ProcessAccessRequestUseCase processAccessRequestUseCase = new ProcessAccessRequestUseCase();
	
	  /*
     * Libera o acesso na catraca padrao conectada.
     * Caso nao seja catraca ou nao esteja conectada,
     * entao exibe uma janela para selecionar uma catraca
     * ou escolhe a unica catraca conectada.
     */
	public void execute(final String idPedestrian, final String motivoLiberacao) {
		SwingWorker<Void, Void> worker = getWorker(idPedestrian, motivoLiberacao);
        worker.execute();
	}
	
	public static synchronized boolean getApertouF9() {
		return apertouF9;
	}
	
	public static synchronized void setApertouF9(final boolean status) {
		apertouF9 = status;
	}
	
	public static synchronized boolean getApertouF10() {
		return apertouF10;
	}
	
	public static synchronized void setApertouF10(final boolean status) {
		apertouF10 = status;
	}
	
	private SwingWorker<Void, Void> getWorker(final String idPedestrian, final String motivoLiberacao) {
		return new SwingWorker<Void, Void>() {
            
			@Override
            public Void doInBackground() {
                if (Objects.isNull(Main.devicesList) || Main.devicesList.isEmpty()) {
                    if (Utils.getPreferenceAsBoolean("registerAccessWithoutConnectedDevices")
                            && Objects.nonNull(idPedestrian)) {
                    	registraAcesso(idPedestrian, motivoLiberacao);
                        return null;
                    }

                    Utils.createNotification("Sem dispositivos conectados.", NotificationType.BAD);
                    Utils.sleep(1000);
                    apertouF9 = false;
                    apertouF10 = false;

                    return null;
                }

                Device selectedDevice = null;
                List<Device> dispositivoConectados = new ArrayList<Device>();
                Device defaultDevice = null;
                for (Device device : Main.devicesList) {
                    if (device.isConnected()) {
                        dispositivoConectados.add(device);
                        if (device.isDefaultDevice()) {
                        	defaultDevice = device;
                        }
                    }
                }

                if (defaultDevice != null) {
                    selectedDevice = defaultDevice;

                } else if (dispositivoConectados.isEmpty()) {
                    Utils.createNotification("Sem catracas conectadas.", NotificationType.BAD);

                } else if (dispositivoConectados.size() == 1) {
                    selectedDevice = dispositivoConectados.get(0);

                } else {
                    ReleaseAccessDialog releaseAccessDialog = new ReleaseAccessDialog(Main.mainScreen, dispositivoConectados);
                    
                    releaseAccessDialog.setVisible(true);
                    if ("OK".equals(releaseAccessDialog.getOption())) {
                    	selectedDevice = releaseAccessDialog.getSelectedDevice();
                    }
                }

                if (selectedDevice != null) {
                    final Boolean exigeSenha = Utils.getPreferenceAsBoolean("releaseAccessRequiresPassword");
                    if (Boolean.TRUE.equals(exigeSenha)) {
                        AutenticationDialog autenticationDialog = new AutenticationDialog(null,true,true,true);
                        Boolean retornoAuthentication = null;
                        try {
                            retornoAuthentication = autenticationDialog.authenticate();

                        } catch (Exception ex) {
                            ex.printStackTrace();
                            JOptionPane.showMessageDialog(null, "Ocorreu uma falha ao validar a senha.",
                                    "Erro na validacao", JOptionPane.PLAIN_MESSAGE);
                            return null;
                        }

                        if (Objects.isNull(retornoAuthentication)) {
                        	return null;
                        }

                        if (Boolean.FALSE.equals(retornoAuthentication)) {
                            JOptionPane.showMessageDialog(null, "Nao foi possssivel validar a senha, ou senha invalida",
                                    "Erro na validacao", JOptionPane.PLAIN_MESSAGE);
                            return null;
                        }
                    }

                    String motivos = Utils.getPreferenceWithNull("releaseAccessReason");
                    String newMotivo = null;

                    if (!Utils.isNullOrEmpty(motivos)) {
                        ReleaseReasonDialog releaseReasonDialog = new ReleaseReasonDialog(Main.mainScreen, motivos);
                        Main.releaseReasonDialog = releaseReasonDialog;
                        releaseReasonDialog.setVisible(true);
                        Main.releaseReasonDialog = null;

                        if ("CANCEL".equals(releaseReasonDialog.getOption())) {
                            apertouF9 = false;
                            apertouF10 = false;
                            return null;
                        }

                        newMotivo = releaseReasonDialog.getReason();

                        if (Utils.isNullOrEmpty(newMotivo)) {
                            Utils.createNotification("E necessario informar um motivo.", NotificationType.BAD);
                            Utils.sleep(1000);
                            apertouF9 = false;
                            apertouF10 = false;
                            return null;
                        }
                    }

                    // TUDO PRONTO, PODE LIBERAR
                    final JButton button = Main.mainScreen.getLiberarAcessoButton();

                    final Icon previousIcon = button.getIcon();

                    try {
                    	Main.releaseTicketGateMenuItem.setEnabled(false);
                        button.setEnabled(false);
                        button.setText("Acesso permitido!");
                        Main.mainScreen.getLiberarAcessoMenuItem().setEnabled(false);

                        String direction = apertouF9 ? Tipo.ENTRADA : (apertouF10 ? Tipo.SAIDA : Tipo.ENTRADA);
                        String equipament = selectedDevice.getFullIdentifier();

                        if (idPedestrian != null) {
                            PedestrianAccessEntity athleteAccess = (PedestrianAccessEntity) HibernateAccessDataFacade
                                    .getSingleResultById(PedestrianAccessEntity.class, Long.valueOf(idPedestrian));

                            LogPedestrianAccessEntity logAccess = new LogPedestrianAccessEntity(Main.loggedUser.getId(),
                                    athleteAccess.getId(), "SYSTEM", selectedDevice.getLocation(), newMotivo,
                                    direction, equipament);

                            if (Manufacturer.SERVER.equals(selectedDevice.getManufacturer())) {
                                ((ServerDevice) selectedDevice).setLogAccess(logAccess);

                            } else {
                                Utils.createNotification(
                                        "Usuario " + athleteAccess.getFirstName() + " liberado pelo sistema.",
                                        NotificationType.GOOD);
                                HibernateAccessDataFacade.save(LogPedestrianAccessEntity.class, logAccess);
                                if (Main.broadcastServer != null) {
                                	Main.broadcastServer.sendMessage(
                                			new BroadcastMessageTO(BroadcastMessageType.LOG_ACCESS, logAccess));
                                }
                            }

                        } else {
                            LogPedestrianAccessEntity logAccess = new LogPedestrianAccessEntity(Main.loggedUser.getId(),
                                    null, "LIBERADO PELO SISTEMA", selectedDevice.getLocation(), newMotivo,
                                    direction, equipament);

                            if (Manufacturer.SERVER.equals(selectedDevice.getManufacturer())) {
                                ((ServerDevice) selectedDevice).setLogAccess(logAccess);

                            } else {
                                Utils.createNotification("Acesso liberado pelo sistema.", NotificationType.GOOD);
                                HibernateAccessDataFacade.save(LogPedestrianAccessEntity.class, logAccess);
                            }
                        }
                        selectedDevice.allowAccess();

                        Thread.sleep(3000);

                    } catch (Exception e) {
                        e.printStackTrace();

                    } finally {
                    	Main.releaseTicketGateMenuItem.setEnabled(true);
                        button.setEnabled(true);
                        button.setIcon(previousIcon);
                        button.setText("Liberar acesso (F9)/(F10)");
                        Main.mainScreen.getLiberarAcessoMenuItem().setEnabled(true);
                        apertouF9 = false;
                        apertouF10 = false;
                    }

                    return null;

                } else {
                    Utils.sleep(1000);
                    apertouF9 = false;
                    apertouF10 = false;
                }

                return null;
            }
			
			private void registraAcesso(final String idPedestrian, final String motivoLiberacao) {
				try {
                    final Object[] resultado = processAccessRequestUseCase.processAccessRequest(idPedestrian, null,
                            Origens.ORIGEM_LIBERADO_SISTEMA, null, false, true, false);

                    final VerificationResult verificationResult = (VerificationResult) resultado[0];
                    final PedestrianAccessEntity matchedPedestre = (PedestrianAccessEntity) resultado[2];

                    if (!VerificationResult.ALLOWED.equals(verificationResult)) {
                        return;
                    }
                    
                    final LogPedestrianAccessEntity logAccess = new LogPedestrianAccessEntity(Main.loggedUser.getId(),
                            matchedPedestre.getId(), "SYSTEM", null, motivoLiberacao);
                    Utils.createNotification("Usuario " + matchedPedestre.getFirstName() + " liberado pelo sistema.", NotificationType.GOOD);
                    HibernateAccessDataFacade.save(LogPedestrianAccessEntity.class, logAccess);
                    Thread.sleep(1000);

                    matchedPedestre.decrementaCreditos();
                    HibernateAccessDataFacade.save(PedestrianAccessEntity.class, matchedPedestre);

                } catch (Exception e) {
                    e.printStackTrace();
                }
			}
			
        };
	}
}
