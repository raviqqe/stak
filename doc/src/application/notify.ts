export const notify = async (message: string): Promise<void> => {
  await requestNotificationPermission();

  if (Notification.permission === "denied") {
    return;
  }

  new Notification(message);
};

export const requestNotificationPermission = async (): Promise<void> => {
  if (
    Notification.permission !== "denied" &&
    Notification.permission !== "granted"
  ) {
    await Notification.requestPermission();
  }
};
